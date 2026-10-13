use super::{random, split_for_impl};
use crate::supertrait::{
    process_supported_supertraits, reject_generic_supertraits, validate_trait,
};
use announce::helper::{Cont, Entry, Gather, KvArgs};
use proc_macro2::Span;
use proc_macro2::TokenStream;
use proc_macro_error::abort;
use std::collections::HashMap;
use syn::parse::Parse;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::*;
use template_quote::quote;
use template_quote::ToTokens;

#[derive(syn_derive::Parse)]
struct IdentAndType {
    ident: Ident,
    _colon_token: Token![:],
    ty: Type,
}

impl ToTokens for IdentAndType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.ident.to_tokens(tokens);
        self.ty.to_tokens(tokens);
    }
}
/// The `REST` of the trampoline: the `#[sumtype]` site's context, written by `gen_trait_impls`.
struct Context {
    krate: Path,
    bounds: Punctuated<Bound, Token![,]>,
    enum_path: Path,
    unspecified_ty_params: Punctuated<Ident, Token![,]>,
    variants: Punctuated<IdentAndType, Token![,]>,
    impl_generics_base: Punctuated<GenericParam, Token![,]>,
    ty_generics_base: Punctuated<GenericArgument, Token![,]>,
    where_clause_base: Punctuated<WherePredicate, Token![,]>,
}

#[derive(syn_derive::Parse)]
struct Bound(Path, #[allow(dead_code)] Token![=>], Ident);

fn list<T: Parse>(kv: &mut KvArgs, key: &str) -> Result<Punctuated<T, Token![,]>> {
    Ok(kv.list::<T>(key)?.unwrap_or_default().into_iter().collect())
}

impl Context {
    fn parse(rest: &announce::helper::Input) -> Result<Self> {
        let mut kv: KvArgs = parse2(rest.rest_as::<proc_macro2::Group>()?.stream())?;
        let ctx = Context {
            krate: kv.require("krate")?,
            bounds: list(&mut kv, "bounds")?,
            enum_path: kv.require("enum_path")?,
            unspecified_ty_params: list(&mut kv, "unspecified_ty_params")?,
            variants: list(&mut kv, "variants")?,
            impl_generics_base: list(&mut kv, "impl_generics_base")?,
            ty_generics_base: list(&mut kv, "ty_generics_base")?,
            where_clause_base: list(&mut kv, "where_clause_base")?,
        };
        kv.finish().map(|_| ctx)
    }
}

fn has_self_ty(ty: &Type) -> Option<Span> {
    use syn::visit::Visit;
    struct Visitor(Option<Span>);
    impl syn::visit::Visit<'_> for Visitor {
        fn visit_type(&mut self, i: &Type) {
            if let Type::Path(tp) = i {
                if tp.path.is_ident("Self") {
                    self.0 = Some(tp.path.span());
                    return;
                }
            }
            syn::visit::visit_type(self, i)
        }
    }
    let mut visitor = Visitor(None);
    visitor.visit_type(ty);
    visitor.0
}

pub(crate) fn generate_fn_args(
    sig: &Signature,
    self_arg_id: &Ident,
) -> (Vec<(FnArg, Ident)>, Option<Ident>, bool) {
    let mut recv: Option<Ident> = None;
    let mut ret = Vec::new();
    for (n, input) in sig.inputs.iter().enumerate() {
        match input {
            FnArg::Receiver(receiver) => {
                let ident = Ident::new("self", receiver.self_token.span());
                if let Some(recv) = &recv {
                    abort!(
                        recv.span().join(receiver.span()).unwrap_or(receiver.span()),
                        "Cannot have multiple Self type inputs in a trait method signature."
                    )
                } else {
                    recv = Some(ident.clone());
                }
                ret.push((input.clone(), self_arg_id.clone()));
            }
            FnArg::Typed(pt) => {
                let ident = match pt.pat.as_ref() {
                    Pat::Ident(pi) if &pi.ident != "_" => pi.ident.clone(),
                    _ => Ident::new(&format!("__sumtype_arg_{}", n), pt.span()),
                };
                // Check if supported Self type?
                if pt.ty.as_ref() == &parse_quote!(Self)
                    || pt.ty.as_ref() == &parse_quote!(&Self)
                    || pt.ty.as_ref() == &parse_quote!(&mut Self)
                {
                    if recv.is_some() {
                        abort!(
                            pt.pat.span(),
                            "Cannot have multiple Self type inputs in a trait method signature."
                        )
                    } else {
                        recv = Some(ident.clone());
                    }
                } else if let Some(loc) = has_self_ty(&pt.ty) {
                    abort!(loc, "Cannot use `Self` type here. Only `Self`, `&Self`, `&mut Self` is supported.");
                }

                let mut pt = pt.clone();
                pt.pat = Box::new(Pat::Ident(PatIdent {
                    ident: ident.clone(),
                    attrs: vec![],
                    by_ref: None,
                    mutability: None,
                    subpat: None,
                }));
                ret.push((FnArg::Typed(pt), ident));
            }
        }
    }
    let return_is_self = if let ReturnType::Type(_, ty) = &sig.output {
        if ty.as_ref() == &parse_quote!(Self) {
            true
        } else if let Some(loc) = has_self_ty(ty.as_ref()) {
            abort!(
                loc,
                "Cannot use conditional `Self` type here. only `Self` is supported."
            )
        } else {
            false
        }
    } else {
        false
    };
    (ret, recv, return_is_self)
}

#[allow(clippy::too_many_arguments)]
fn process_fn(
    item_fn: &TraitItemFn,
    variants: &Punctuated<IdentAndType, Token![,]>,
    iter_ty_generics: &[GenericArgument],
    enum_path: &Path,
    implementation: &Path,
) -> TokenStream {
    let self_arg_ident = Ident::new("__sumtype_self_arg", Span::call_site());
    let (args, recv, return_is_self) = generate_fn_args(&item_fn.sig, &self_arg_ident);
    let (fn_impl_generics, _, fn_where_clause) = item_fn.sig.generics.split_for_impl();
    // Forward the method's own type/const generics to the dispatched call as a turbofish, so a
    // generic that is only supplied via turbofish (e.g. a const param, or a type param used only
    // internally) and is not inferable from the arguments/return type still resolves. Lifetimes
    // are omitted (they cannot be turbofished and are inferred).
    let method_turbofish: Vec<Ident> = item_fn
        .sig
        .generics
        .params
        .iter()
        .filter_map(|p| match p {
            GenericParam::Type(t) => Some(t.ident.clone()),
            GenericParam::Const(c) => Some(c.ident.clone()),
            GenericParam::Lifetime(_) => None,
        })
        .collect();
    quote! {
        #{&item_fn.sig.constness} #{&item_fn.sig.asyncness} #{&item_fn.sig.unsafety}
        #{&item_fn.sig.abi} fn #{&item_fn.sig.ident} #fn_impl_generics (
            #(for (input, _) in &args), { #input }
            #{&item_fn.sig.variadic}
        )
        #{&item_fn.sig.output} #fn_where_clause {
            #(if let Some(recv) = recv) {
                match #recv {
                    #(for IdentAndType{ident, ty, ..} in variants) {
                        #enum_path::#ident(#self_arg_ident) =>
                            #(if return_is_self) {
                                #enum_path::#ident(<#ty as #implementation<
                                     #(#iter_ty_generics),*
                                     >>::#{&item_fn.sig.ident}
                                     #(if !method_turbofish.is_empty()) {
                                         ::<#(#method_turbofish),*>
                                     }
                                     (
                                         #(for (_, arg) in &args), { #arg }
                                )),
                            } #(else) {
                                <#ty as #implementation<
                                     #(#iter_ty_generics),*
                                 >>::#{&item_fn.sig.ident}
                                 #(if !method_turbofish.is_empty()) {
                                     ::<#(#method_turbofish),*>
                                 }
                                 (
                                     #(for (_, arg) in &args), { #arg }
                                ),
                            }
                    }
                    Self::__Uninhabited(_) => ::core::unreachable!(),
                }

            }
        }
    }
}

fn sort_impl_generics(a: &mut [&GenericParam]) {
    // Group as lifetimes < consts < types. A rank-based key gives a proper total order; the
    // previous comparator returned `Less` for both `cmp(a, b)` and `cmp(b, a)` on two distinct
    // lifetimes, violating the strict-weak-ordering contract of `sort_by` (which can panic).
    fn rank(p: &GenericParam) -> u8 {
        match p {
            GenericParam::Lifetime(_) => 0,
            GenericParam::Const(_) => 1,
            GenericParam::Type(_) => 2,
        }
    }
    a.sort_by_key(|p| rank(p));
}

fn arr_to_ts<I: IntoIterator>(a: I) -> impl Iterator<Item = TokenStream>
where
    I::Item: ToTokens,
{
    a.into_iter().map(|a| quote!(#a))
}

/// One fetched trait definition, deduplicated by the trait it implements.
struct Node {
    item_trait: ItemTrait,
    implementation: Path,
    /// Supertraits as `#[sumtype]` invokes them (built-ins mapped to their `announce-std` mocks).
    supertraits: Vec<Path>,
}

fn key_of(p: &Path) -> String {
    quote!(#p).to_string()
}

/// Supertraits of a fetched definition, with the built-in ones mapped to their mocks. The `Sized` of
/// announce-std's `Clone` mock is dropped (every sum type is `Sized`).
fn supertraits_of(entry: &Entry, item: &ItemTrait, krate: &Path) -> Vec<Path> {
    let supers = process_supported_supertraits(
        item.supertraits.iter().filter(|b| {
            !matches!(b, TypeParamBound::Trait(t) if entry.mock().is_some()
                && matches!(t.path.segments.last(), Some(s) if s.ident == "Sized"))
        }),
        krate,
    );
    reject_generic_supertraits(&supers);
    supers
}

/// `announce-std` mocks give provided methods a placeholder body. Only required methods are
/// forwarded (forwarding e.g. `Iterator::count`/`last` also breaks inference in `tests/bug.rs`),
/// except `Error::source`, whose default (`None`) would hide the variant's source.
fn forwarded_by_mock(mock: &Path, f: &TraitItemFn) -> bool {
    f.default.is_none()
        || (f.sig.ident == "source"
            && matches!(mock.segments.last(), Some(s) if s.ident == "Error"))
}

/// The trait of an entry: an announced trait is delivered as written (announce-std mocks and
/// `#[announce]` traits alike). A library that resolves the paths of a trait itself can announce
/// a copy wrapped in an inline module of the same name (`#[announce(emit_item = false)] mod T {
/// trait T {..} }`): its relative paths then resolve at the `#[sumtype]` site, and the module
/// is unwrapped here. A trait announced with `repeater = ..` has its relative types rewritten to
/// the repeater projections (its `type_leak` META), so they resolve here.
fn trait_of(entry: &Entry) -> Option<ItemTrait> {
    let mut item = parse2::<Item>(entry.item.clone()).ok()?;
    if let Some(type_leak) = entry.type_leak().ok().flatten() {
        type_leak.apply(&mut item);
    }
    match item {
        Item::Trait(t) => Some(t),
        Item::Mod(ItemMod {
            content: Some((_, items)),
            ..
        }) => match <[Item; 1]>::try_from(items) {
            Ok([Item::Trait(t)]) => Some(t),
            _ => None,
        },
        _ => None,
    }
}

fn node_of(entry: &Entry, krate: &Path) -> Result<Node> {
    let mut item_trait: ItemTrait = trait_of(entry).unwrap_or_else(|| {
        abort!(
            entry.key.path().span(),
            "`{}` is not a trait; `#[sumtype]` bounds must be announced traits (`#[announce]`, or `announce_std::iter::Iterator` and the like)",
            key_of(&entry.key.path())
        )
    });
    let supertraits = supertraits_of(entry, &item_trait, krate);
    let implementation = match entry.mock() {
        // A mock (`announce-std`, or `#[announce(mock = path)]`): implement the real trait.
        Some(mock) => {
            item_trait.items.retain(|i| match i {
                TraitItem::Fn(f) => forwarded_by_mock(&mock, f),
                _ => true,
            });
            mock
        }
        // Otherwise implement the trait itself, by the path its trampoline was invoked through
        // (it resolves at the `#[sumtype]` site). `sumtype::check` runs the same validation at
        // the definition.
        None => {
            validate_trait(&item_trait, krate);
            entry.key.path()
        }
    };
    Ok(Node {
        item_trait,
        implementation,
        supertraits,
    })
}

/// Receives `[_sumtype_internal!] ENTRY* { context }`: the definitions of every `#[sumtype]`
/// bound (and, after further rounds, of their supertraits), and emits the impls.
pub fn sumtype_internal(input: announce::helper::Input) -> Result<TokenStream> {
    let ctx = Context::parse(&input)?;
    let krate = &ctx.krate;

    // Fetch the supertraits that have not arrived yet with one more trampoline round: a single
    // longer continuation list instead of one recursive call per supertrait.
    let needed = ctx.bounds.iter().map(|b| b.0.clone()).chain(
        input
            .entries_in_order()
            .filter_map(|e| Some((e, trait_of(e)?)))
            .flat_map(|(e, item)| supertraits_of(e, &item, krate)),
    );
    if let Gather::Fetch(ts) = input.gather(needed.map(Cont::bang).collect::<Vec<_>>()) {
        return Ok(ts);
    }

    // All definitions are here. One node per implemented trait, so a trait reached twice (a
    // repeated bound, or a supertrait diamond) is implemented once. User traits are keyed by
    // their trampoline's path (the entry `Cont`), so two different traits with the same name
    // stay apart.
    let mut nodes: Vec<Node> = Vec::new();
    let mut node_of_impl: HashMap<String, usize> = HashMap::new();
    let mut node_of_key: HashMap<Cont, usize> = HashMap::new();
    for e in input.entries_in_order() {
        let node = node_of(e, krate)?;
        let n = *node_of_impl
            .entry(key_of(&node.implementation))
            .or_insert_with(|| {
                nodes.push(node);
                nodes.len() - 1
            });
        node_of_key.insert(e.key.clone(), n);
    }
    // Constraint traits: a bound's node takes the bound's name (used by the `sumtype!(..)`
    // expression check); a repeated bound gets an alias; supertrait-only nodes get fresh names.
    let mut cts: Vec<Option<Ident>> = vec![None; nodes.len()];
    let mut aliases: Vec<(Ident, usize)> = Vec::new();
    for Bound(p, _, ct) in &ctx.bounds {
        let n = node_of_key[&Cont::bang(p.clone())];
        if cts[n].is_none() {
            cts[n] = Some(ct.clone());
        } else {
            aliases.push((ct.clone(), n));
        }
    }
    let cts: Vec<Ident> = cts
        .into_iter()
        .enumerate()
        .map(|(n, ct)| {
            ct.unwrap_or_else(|| {
                Ident::new(
                    &format!("__SumTrait_ConstraintTrait_{}_{}", n, random()),
                    Span::call_site(),
                )
            })
        })
        .collect();
    let ty_generics_base = &ctx.ty_generics_base;
    let ty_generics_enum = ctx
        .ty_generics_base
        .iter()
        .map(|a| quote!(#a))
        .chain(ctx.unspecified_ty_params.iter().map(|i| quote!(#i)))
        .collect::<Vec<_>>();
    let where_clause_base = &ctx.where_clause_base;
    let enum_path = &ctx.enum_path;
    Ok(quote! {
        #(for (n, node) in nodes.iter().enumerate()) {
            #{
                let super_cts = node
                    .supertraits
                    .iter()
                    .map(|s| cts[node_of_key[&Cont::bang(s.clone())]].clone())
                    .collect::<Vec<_>>();
                emit_node(&ctx, node, &cts[n], &super_cts)
            }
        }
        #(for (alias, n) in &aliases) {
            #[allow(non_camel_case_types)]
            trait #alias<#{&ctx.impl_generics_base}> {}
            impl<
                #(for p in &ctx.impl_generics_base) { #p, }
                #(for p in &ctx.unspecified_ty_params) { #p, }
            > #alias<#ty_generics_base> for #enum_path<#(#ty_generics_enum),*>
            where
                #where_clause_base
                #(if !where_clause_base.is_empty()) { , }
                Self: #{&cts[*n]}<#ty_generics_base>,
            {}
        }
    })
}

/// The constraint trait and the dispatching impl for one trait definition.
fn emit_node(
    ctx: &Context,
    node: &Node,
    constraint_expr_trait_ident: &Ident,
    supertraits_constraint_traits: &[Ident],
) -> TokenStream {
    let Context {
        enum_path,
        unspecified_ty_params,
        variants,
        impl_generics_base,
        ty_generics_base,
        where_clause_base,
        ..
    } = ctx;
    let Node {
        item_trait,
        implementation,
        supertraits,
    } = node;
    let mut impl_generics_merged = item_trait
        .generics
        .params
        .iter()
        .chain(impl_generics_base)
        .collect::<Vec<_>>();
    sort_impl_generics(impl_generics_merged.as_mut_slice());
    let impl_generics_base = arr_to_ts(impl_generics_base).collect::<Vec<_>>();
    let assoc_types = item_trait
        .items
        .iter()
        .filter_map(|item| match item {
            TraitItem::Type(c) => Some((
                Ident::new(
                    &format!("__SumType_AssocType_{}", c.ident),
                    Span::call_site(),
                ),
                c.clone(),
            )),
            _ => None,
        })
        .collect::<Vec<_>>();
    let (_, ty_generics_trait, _) = split_for_impl(Some(&item_trait.generics));
    let ty_generics_base = ty_generics_base
        .iter()
        .map(|a| quote!(#a))
        .collect::<Vec<_>>();
    let ty_generics_enum = ty_generics_base
        .iter()
        .cloned()
        .chain(unspecified_ty_params.iter().map(|ident| quote!(#ident)))
        .collect::<Vec<_>>();
    let impl_generics = impl_generics_merged
        .iter()
        .map(|i| quote!(#i))
        .chain(assoc_types.iter().map(|(c, _)| quote!(#c)))
        .chain(unspecified_ty_params.iter().map(|ident| quote!(#ident)))
        .collect::<Vec<_>>();
    quote! {
        #[allow(non_camel_case_types)]
        trait #constraint_expr_trait_ident<#(#impl_generics_base),*> {}
        impl<#(#impl_generics),*> #constraint_expr_trait_ident<#(#ty_generics_base),*>
            for #enum_path <#(#ty_generics_enum),*>
         where
            #where_clause_base
            #(if !where_clause_base.is_empty()) { , }
            #(for IdentAndType{ty, ..} in variants) {
                #ty: #implementation<
                    #(#ty_generics_trait),*
                    #(if !ty_generics_trait.is_empty() && !assoc_types.is_empty()) { , }
                    #(for (atp, at) in &assoc_types),{
                        #{&at.ident} = #atp
                    }
                >,
            }
            #(for constraint_trait in supertraits_constraint_traits) {
                Self: #constraint_trait<#(#ty_generics_base),*>,
            }
        { }
        impl <#(#impl_generics),*> #implementation<#(for p in &item_trait.generics.params),{#p}>
            for #enum_path<#(#ty_generics_enum),*>
        where
            #where_clause_base
            #(if !where_clause_base.is_empty()) { , }
            #(for IdentAndType{ty, ..} in variants) {
                #ty: #implementation<
                    #(#ty_generics_trait),*
                    #(if !ty_generics_trait.is_empty() && !assoc_types.is_empty()) { , }
                    #(for (atp, at) in &assoc_types), {
                        #{&at.ident} = #atp
                    }
                >,
            }
            #(for supertrait in supertraits) {
                Self: #supertrait,
            }
        {
            #(for (atp, at) in &assoc_types) {
                #(let (at_impl_trait, _, at_where) = at.generics.split_for_impl()) {
                    type #{&at.ident} #at_impl_trait = #atp #at_where;
                }
            }
            #(for f in item_trait.items.iter().filter_map(|item| {
                if let TraitItem::Fn(f) = item { Some(f) } else { None }}
            )) {
                #{process_fn(f, variants, &ty_generics_trait, enum_path, implementation)}
            }
        }
    }
}
