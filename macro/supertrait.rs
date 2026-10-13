//! What `#[sumtype]` supports in a trait it receives from an announce trampoline: the supertraits
//! (built-in `std` traits are mapped to their `announce-std` mocks) and the shape of the items.
//! The same rules are applied by `sumtype::check` at the trait's definition and, for traits that
//! were not checked there, by `#[sumtype]` when it receives the definition.

use proc_macro_error::abort;
use syn::spanned::Spanned;
use syn::*;

use crate::sumtype_internal::generate_fn_args;

/// The standard-library traits that have an `announce-std` mock, keyed by the `(module, Trait)`
/// segments of their canonical `core`/`std` path (which are also the mock's `announce_std` path). Used to recognize a *qualified* supertrait path
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
/// Used to avoid mis-mapping a user trait whose module/name merely collides with a std
/// trait (e.g. `crate::hash::Hash`, `mymod::fmt::Debug`) to the std-backed mock.
fn is_std_rooted(path: &Path) -> bool {
    path.segments.first().map_or(false, |s| {
        matches!(s.ident.to_string().as_str(), "core" | "std" | "alloc")
    })
}

/// If `path` denotes a built-in mock-able std/core trait, returns the `(module, Trait)` of its
/// `announce-std` mock. A *qualified* path is recognized only when it is rooted at the standard
/// library and its trailing `(module, Trait)` matches a canonical location; a *bare* ident is
/// recognized only for the prelude markers `Copy`/`Clone` (which unambiguously work unqualified).
/// Anything else — an announced user trait by bare name or path, an `announce_std::…` path, or
/// `std::…::Debug` written via an alias — is left for the caller to forward as a user trait, so a
/// same-named user trait is never hijacked.
fn map_builtin_supertrait(path: &Path) -> Option<(Ident, Ident)> {
    let last = path.segments.last()?;
    let name = last.ident.to_string();
    if path.segments.len() == 1 {
        // Bare ident: only the prelude markers that are unambiguous unqualified. `Debug`/`Hash`
        // bare resolve to the derive macro; `Display`/`Error`/`Read`/`Iterator` should be qualified
        // to disambiguate from a same-named user trait.
        let module = match name.as_str() {
            "Copy" => "marker",
            "Clone" => "clone",
            _ => return None,
        };
        return Some((Ident::new(module, last.ident.span()), last.ident.clone()));
    }
    if !is_std_rooted(path) {
        return None;
    }
    let n = path.segments.len();
    let module = path.segments[n - 2].ident.to_string();
    BUILTIN_SUPERTRAITS
        .iter()
        .any(|(m, t)| *m == module && *t == name)
        .then(|| (path.segments[n - 2].ident.clone(), last.ident.clone()))
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
                "only trait bounds are supported as `#[sumtype]` supertraits"
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
                "`{}` is not supported as a supertrait of a `#[sumtype]` trait (the generated sum type cannot derive it; supported supertraits are `Copy`, `Clone`, `Hash`, `Debug`, `Display`, `Error`, `Iterator`, `Read`, and other announced traits)",
                ident
            );
        }
        if let Some((module, mapped)) = map_builtin_supertrait(&tb.path) {
            // A built-in std/core trait → its `announce-std` mock, through sumtype's hidden
            // re-export so that the user does not have to depend on `announce-std`.
            supertraits.push(parse_quote!(#krate::__private::announce_std::#module::#mapped));
        } else {
            // Otherwise assume an announced user trait (bare ident or path); it is invoked as
            // its dispatch macro during expansion. Previously bare idents here were silently
            // dropped, leaving the supertrait unimplemented.
            supertraits.push(tb.path.clone());
        }
    }
    supertraits
}

/// Rejects a supertrait with generic arguments (`Foo<T>`).
pub(crate) fn reject_generic_supertraits(supers: &[Path]) {
    for p in supers {
        if p.segments.iter().any(|s| !s.arguments.is_empty()) {
            // The span of the first segment that has arguments (a single token, so the position
            // does not depend on whether the compiler can join spans).
            let seg = p.segments.iter().find(|s| !s.arguments.is_empty());
            abort!(
                seg.map_or_else(|| p.span(), |s| s.ident.span()),
                "generic supertraits are not supported by `#[sumtype]`"
            );
        }
    }
}

/// Validates a trait for `#[sumtype]`: the supertraits and the shape of the items (the rules the
/// docs of `sumtype::check` list). Aborts with an error at the offending item.
pub(crate) fn validate_trait(input: &ItemTrait, krate: &Path) {
    // Rejects `Eq`/`PartialEq` and non-trait bounds.
    let supers = process_supported_supertraits(&input.supertraits, krate);
    reject_generic_supertraits(&supers);
    for item in &input.items {
        match item {
            TraitItem::Const(c) => abort!(
                c.const_token.span,
                "associated const is not supported by `#[sumtype]`"
            ),
            TraitItem::Fn(tfn) => {
                if tfn.sig.inputs.is_empty() || !matches!(&tfn.sig.inputs[0], FnArg::Receiver(_)) {
                    abort!(
                        tfn.sig.ident.span(),
                        "`#[sumtype]` requires a receiver (`self`, `&self` or `&mut self`) as the first parameter"
                    )
                }
                // Aborts on a `Self` that cannot be dispatched (`Vec<Self>`, a second `Self`
                // argument, ...).
                let _ = generate_fn_args(&tfn.sig, &Ident::new("__self", tfn.sig.ident.span()));
            }
            TraitItem::Type(tty) => {
                if tty.default.is_some() {
                    abort!(
                        tty.ident.span(),
                        "associated type defaults are not supported by `#[sumtype]`"
                    )
                }
                if !tty.generics.params.is_empty() || tty.generics.where_clause.is_some() {
                    abort!(
                        tty.ident.span(),
                        "generic associated types are not supported by `#[sumtype]`"
                    )
                }
            }
            o => abort!(
                o.span(),
                "only methods and associated types are supported by `#[sumtype]`"
            ),
        }
    }
}
