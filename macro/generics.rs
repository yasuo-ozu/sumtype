//! Pure helpers for manipulating generic parameters/arguments and building simple paths.

use proc_macro2::Span;
use syn::*;
use template_quote::{quote, ToTokens};

/// A fresh, effectively-random `u64`, used to mint collision-free identifiers per expansion.
pub(crate) fn random() -> u64 {
    use std::hash::{BuildHasher, Hasher};
    std::collections::hash_map::RandomState::new()
        .build_hasher()
        .finish()
}

pub(crate) fn generic_param_to_arg(i: GenericParam) -> GenericArgument {
    match i {
        GenericParam::Lifetime(LifetimeParam { lifetime, .. }) => {
            GenericArgument::Lifetime(lifetime)
        }
        GenericParam::Type(TypeParam { ident, .. }) => GenericArgument::Type(parse_quote!(#ident)),
        GenericParam::Const(ConstParam { ident, .. }) => {
            GenericArgument::Const(parse_quote!(#ident))
        }
    }
}

/// Dedup key for a generic *parameter*, by its declared name.
fn generic_param_key(p: &GenericParam) -> String {
    match p {
        GenericParam::Lifetime(l) => format!("'{}", l.lifetime.ident),
        GenericParam::Type(t) => t.ident.to_string(),
        GenericParam::Const(c) => c.ident.to_string(),
    }
}

/// Merge two generic-parameter lists into the canonical order (lifetimes, then consts, then
/// types), **deduplicating by name** and unioning bounds. Without the dedup, a
/// `sumtype!(.., for<'a, T>)` that repeats the enclosing item's own generics would emit two
/// params with the same name (E0403). See `tests/compile_fail.md` / `tests/fixed_bugs.rs`.
pub(crate) fn merge_generic_params(
    args1: impl IntoIterator<Item = GenericParam>,
    args2: impl IntoIterator<Item = GenericParam>,
) -> impl Iterator<Item = GenericParam> {
    // Canonical order: lifetimes, then consts, then types; `v1` before `v2` within each kind. A
    // *stable* sort by kind rank over `v1 ++ v2` preserves exactly that ordering.
    fn kind_rank(p: &GenericParam) -> u8 {
        match p {
            GenericParam::Lifetime(_) => 0,
            GenericParam::Const(_) => 1,
            GenericParam::Type(_) => 2,
        }
    }
    let mut ordered: Vec<GenericParam> = args1
        .into_iter()
        .chain(args2)
        .collect();
    ordered.sort_by_key(kind_rank);
    let mut merged: Vec<GenericParam> = Vec::new();
    for p in ordered {
        if let Some(existing) = merged
            .iter_mut()
            .find(|e| generic_param_key(e) == generic_param_key(&p))
        {
            match (existing, &p) {
                (GenericParam::Type(e), GenericParam::Type(n)) => {
                    for b in n.bounds.iter() {
                        if !e
                            .bounds
                            .iter()
                            .any(|x| quote!(#x).to_string() == quote!(#b).to_string())
                        {
                            e.bounds.push(b.clone());
                        }
                    }
                }
                (GenericParam::Lifetime(e), GenericParam::Lifetime(n)) => {
                    for b in n.bounds.iter() {
                        if !e.bounds.iter().any(|x| x.ident == b.ident) {
                            e.bounds.push(b.clone());
                        }
                    }
                }
                _ => {}
            }
        } else {
            merged.push(p);
        }
    }
    merged.into_iter()
}

/// Dedup key for the "plain" generic *argument* kinds (lifetime / bare-path type / const ident).
/// Returns `None` for kinds we never dedup (complex types, assoc/constraint), keeping them all.
fn generic_arg_key(a: &GenericArgument) -> Option<String> {
    match a {
        GenericArgument::Lifetime(l) => Some(format!("'{}", l.ident)),
        GenericArgument::Type(Type::Path(tp)) if tp.qself.is_none() => {
            tp.path.get_ident().map(|i| i.to_string())
        }
        _ => None,
    }
}

/// Merge two generic-*argument* lists into canonical order (lifetimes, consts, types, then
/// assoc/constraint), deduplicating named args so an explicit `sumtype!['a, T]` that repeats the
/// enclosing item's own generics does not instantiate the enum with twice as many args (E0107).
pub(crate) fn merge_generic_args(
    args1: impl IntoIterator<Item = GenericArgument>,
    args2: impl IntoIterator<Item = GenericArgument>,
) -> impl Iterator<Item = GenericArgument> {
    // Canonical order: lifetimes, consts, types, then assoc/constraint; `v1` before `v2` within
    // each kind, preserved by a *stable* sort by kind rank over `v1 ++ v2`.
    fn kind_rank(a: &GenericArgument) -> u8 {
        match a {
            GenericArgument::Lifetime(_) => 0,
            GenericArgument::Const(_) => 1,
            GenericArgument::Type(_) => 2,
            // AssocType / AssocConst / Constraint (and any future kind) sort last.
            _ => 3,
        }
    }
    let mut ordered: Vec<GenericArgument> = args1.into_iter().chain(args2).collect();
    ordered.sort_by_key(kind_rank);
    let mut seen: Vec<String> = Vec::new();
    ordered.into_iter().filter(move |a| match generic_arg_key(a) {
        Some(k) => {
            if seen.contains(&k) {
                false
            } else {
                seen.push(k);
                true
            }
        }
        None => true,
    })
}

/// Build a single-segment path `ident`, optionally prefixed with `super::` (used when the
/// `#[sumtype]` context is a module and generated items live one level up).
pub(crate) fn path_of_ident(ident: Ident, is_super: bool) -> Path {
    let mut segments = vec![];
    if is_super {
        segments.push(PathSegment {
            ident: Ident::new("super", Span::call_site()),
            arguments: PathArguments::None,
        });
    }
    segments.push(PathSegment {
        ident,
        arguments: PathArguments::None,
    });
    Path {
        leading_colon: None,
        segments: segments.into_iter().collect(),
    }
}

/// Like [`Generics::split_for_impl`], but returns owned `Vec`s of the impl params, the type-side
/// arguments, and the where-predicates (empty when `generics` is `None`).
pub(crate) fn split_for_impl(
    generics: Option<&Generics>,
) -> (Vec<GenericParam>, Vec<GenericArgument>, Vec<WherePredicate>) {
    if let Some(generics) = generics {
        let (_, ty_generics, where_clause) = generics.split_for_impl();
        let ty_generics: std::result::Result<AngleBracketedGenericArguments, _> =
            parse2(ty_generics.into_token_stream());
        (
            generics.params.iter().cloned().collect(),
            ty_generics
                .map(|g| g.args.into_iter().collect())
                .unwrap_or(vec![]),
            where_clause
                .map(|w| w.predicates.iter().cloned().collect())
                .unwrap_or(vec![]),
        )
    } else {
        (vec![], vec![], vec![])
    }
}
