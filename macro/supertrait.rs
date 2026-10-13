//! The `#[sumtrait]` machinery: analyzing a user trait's supertraits, collecting the leaked types
//! it references, and marking the trait `#[announce]` so `#[sumtype]` can fetch its definition.

use proc_macro2::TokenStream;
use proc_macro_error::abort;
use syn::spanned::Spanned;
use syn::*;
use template_quote::quote;

/// The standard-library traits that have a `sumtype::traits::*` mock, keyed by the `(module, Trait)`
/// segments of their canonical `core`/`std` path. Used to recognize a *qualified* supertrait path
/// (`std::fmt::Debug`); see [`map_builtin_supertrait`].
const BUILTIN_SUPERTRAITS: &[(&str, &str)] = &[
    ("marker", "Copy"),
    ("clone", "Clone"),
    ("hash", "Hash"),
    ("fmt", "Debug"),
    ("fmt", "Display"),
    ("error", "Error"),
    ("iter", "Iterator"),
    ("io", "Read"),
];

/// True when `path`'s first segment is `core`/`std`/`alloc` (with or without a leading `::`), i.e.
/// it is rooted at the standard library rather than a user path (`crate::…`, `mymod::…`, `self::…`).
/// Used to avoid mis-mapping a user `#[sumtrait]` whose module/name merely collides with a std
/// trait (e.g. `crate::hash::Hash`, `mymod::fmt::Debug`) to the std-backed mock.
fn is_std_rooted(path: &Path) -> bool {
    path.segments.first().map_or(false, |s| {
        matches!(s.ident.to_string().as_str(), "core" | "std" | "alloc")
    })
}

/// If `path` denotes a built-in mock-able std/core trait, returns the `sumtype::traits::*` ident it
/// maps to. A *qualified* path is recognized only when it is rooted at the standard library and its
/// trailing `(module, Trait)` matches a canonical location; a *bare* ident is recognized only for
/// the prelude markers `Copy`/`Clone` (which unambiguously work unqualified). Anything else — a user
/// `#[sumtrait]` trait by bare name or path, or `std::…::Debug` written via an alias — is left for
/// the caller to forward as a user trait, so a same-named user trait is never hijacked.
fn map_builtin_supertrait(path: &Path) -> Option<Ident> {
    let last = path.segments.last()?;
    let name = last.ident.to_string();
    if path.segments.len() == 1 {
        // Bare ident: only the prelude markers that are unambiguous unqualified. `Debug`/`Hash`
        // bare resolve to the derive macro; `Display`/`Error`/`Read`/`Iterator` should be qualified
        // to disambiguate from a same-named user trait.
        return matches!(name.as_str(), "Copy" | "Clone").then(|| last.ident.clone());
    }
    if !is_std_rooted(path) {
        return None;
    }
    let n = path.segments.len();
    let module = path.segments[n - 2].ident.to_string();
    BUILTIN_SUPERTRAITS
        .iter()
        .any(|(m, t)| *m == module && *t == name)
        .then(|| Ident::new(&name, last.ident.span()))
}

/// Detects the std `Eq`/`PartialEq` supertrait — bare, or via its canonical `core::cmp`/`std::cmp`
/// path — which cannot be supported (see the comment at the rejection site). A user trait named
/// `Eq`/`PartialEq` reached by a non-std path (e.g. `crate::cmp::Eq`) is *not* matched.
fn unsupported_eq_supertrait(path: &Path) -> Option<Ident> {
    let last = path.segments.last()?;
    let name = last.ident.to_string();
    if name != "Eq" && name != "PartialEq" {
        return None;
    }
    let n = path.segments.len();
    if n == 1 || (is_std_rooted(path) && path.segments[n - 2].ident == "cmp") {
        Some(last.ident.clone())
    } else {
        None
    }
}

pub(crate) fn process_supported_supertraits<'a>(
    traits: impl IntoIterator<Item = &'a TypeParamBound>,
    krate: &Path,
) -> Vec<Path> {
    let mut supertraits = Vec::new();
    for tpb in traits.into_iter() {
        let tb = if let TypeParamBound::Trait(tb) = tpb {
            tb
        } else {
            abort!(
                tpb.span(),
                "only trait bounds are supported as `#[sumtrait]` supertraits"
            );
        };
        // The internal sealing marker, if ever present, is not a real supertrait.
        if let Some(ident) = tb.path.get_ident() {
            if ident.to_string().starts_with("__SumTrait_Sealed") {
                continue;
            }
        }
        // `Eq`/`PartialEq` cannot be implemented for the generated sum type through the
        // single-receiver dispatch mechanism (`PartialEq::eq(&self, other: &Self)` needs
        // cross-variant logic, and `Eq` requires `PartialEq`). Reject with a clear message rather
        // than emitting a dangling path or silently dropping support.
        if let Some(ident) = unsupported_eq_supertrait(&tb.path) {
            abort!(
                ident.span(),
                "`{}` is not supported as a `#[sumtrait]` supertrait (the generated sum type cannot derive it; supported supertraits are `Copy`, `Clone`, `Hash`, `Debug`, `Display`, `Error`, `Iterator`, `Read`, and other `#[sumtrait]` traits)",
                ident
            );
        }
        if let Some(mapped) = map_builtin_supertrait(&tb.path) {
            // A built-in std/core trait → its `sumtype::traits::*` mock.
            supertraits.push(parse_quote!(#krate::traits::#mapped));
        } else {
            // Otherwise assume a user `#[sumtrait]` trait (bare ident or path); it is invoked as
            // its dispatch macro during expansion. Previously bare idents here were silently
            // dropped, leaving the supertrait unimplemented.
            supertraits.push(tb.path.clone());
        }
    }
    supertraits
}

/// Emits the trait and its trampoline. `#[sumtype]` resolves the trait's paths at its own site
/// (as it always did), so the announced payload is the trait wrapped in a module of the same
/// name: announce does not run its type-context check on inline modules, which would reject
/// relative trait bounds such as `trait T: Base` or `fn f<W: Write>(..)` even with a `repeater`
/// (type-leak cannot intern trait bounds). `emit_item = false`: only the trampoline is emitted;
/// the receiver unwraps the module (`sumtrait_internal::trait_of`).
pub(crate) fn sumtrait_impl(args: Option<Path>, krate: &Path, input: ItemTrait) -> TokenStream {
    // Validate the supertraits at the definition site (rejects `Eq`/`PartialEq`, non-trait
    // bounds). The mapping itself is redone by `#[sumtype]` from the fetched definition.
    let _ = process_supported_supertraits(&input.supertraits, krate);
    for item in &input.items {
        match item {
            TraitItem::Const(_) => abort!(item.span(), "associated const is not supported"),
            TraitItem::Fn(tfn) => {
                if tfn.sig.inputs.is_empty() || !matches!(&tfn.sig.inputs[0], FnArg::Receiver(_)) {
                    abort!(tfn.sig.span(), "requires receiver")
                }
            }
            TraitItem::Type(tty) => {
                if tty.default.is_some() {
                    abort!(tty.span(), "associated type defaults is not supported")
                }
                if !tty.generics.params.is_empty() || tty.generics.where_clause.is_some() {
                    abort!(
                        tty.generics.span(),
                        "generalized associated types is not supported"
                    )
                }
            }
            o => abort!(o.span(), "Not supported"),
        }
    }
    let announce = quote!(#krate::__private::announce);
    let (vis, ident) = (&input.vis, &input.ident);
    quote! {
        #input
        // The trampoline: `#[announce]` emits the `TraitName!` macro. `implement = ..` rides
        // along in the entry META (`meta = ..` is never invoked).
        #[#announce::announce(
            crate = #announce,
            emit_item = false,
            #(if let Some(m) = &args) { meta = sumtrait_meta(implement = #m), }
        )]
        #vis mod #ident { #input }
    }
}
