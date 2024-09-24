use super::{random, split_for_impl};
use proc_macro2::Span;
use proc_macro2::TokenStream;
use proc_macro_error::abort;
use std::collections::HashMap;
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
#[derive(syn_derive::Parse)]
struct SumtraitInternalContextInner {
    constraint_expr_trait_ident: Ident,
    _comma_61: Token![,],
    trait_path: Path,
    _comma_0: Token![,],
    enum_path: Path,
    _comma_1: Token![,],
    #[syn(bracketed)]
    _bracket_token0: syn::token::Bracket,
    #[syn(in = _bracket_token0)]
    #[parse(Punctuated::parse_terminated)]
    unspecified_ty_params: Punctuated<Ident, Token![,]>,
    _comma_2: Token![,],
    #[syn(bracketed)]
    _bracket_token1: syn::token::Bracket,
    #[syn(in = _bracket_token1)]
    #[parse(Punctuated::parse_terminated)]
    variants: Punctuated<IdentAndType, Token![,]>,
    _comma_3: Token![,],
    #[syn(bracketed)]
    _bracket_token2: syn::token::Bracket,
    #[syn(in = _bracket_token2)]
    #[parse(Punctuated::parse_terminated)]
    impl_generics_base: Punctuated<GenericParam, Token![,]>,
    _comma_4: Token![,],
    #[syn(bracketed)]
    _bracket_token3: syn::token::Bracket,
    #[syn(in = _bracket_token3)]
    #[parse(Punctuated::parse_terminated)]
    ty_generics_base: Punctuated<GenericArgument, Token![,]>,
    _comma_5: Token![,],
    #[syn(braced)]
    _brace_token1: syn::token::Brace,
    #[syn(in = _brace_token1)]
    where_clause_base: TokenStream,
    _comma_6: Token![,],
}

#[derive(syn_derive::Parse)]
struct SumtraitInternalContent {
    #[syn(braced)]
    _brace_token0: syn::token::Brace,
    #[syn(in = _brace_token0)]
    inner: SumtraitInternalContextInner,
    #[syn(bracketed)]
    _bracket_token4: syn::token::Bracket,
    #[syn(in = _bracket_token4)]
    #[parse(Punctuated::parse_terminated)]
    typerefs: Punctuated<Type, Token![,]>,
    _comma_7: Token![,],
    #[syn(braced)]
    _brace_token: syn::token::Brace,
    #[syn(in = _brace_token)]
    item_trait: ItemTrait,
    _comma_8: Token![,],
    typeref_id: LitInt,
    _comma_9: Token![,],
    krate: Path,
    _comma_10: Token![,],
    marker_path: Path,
    _comma_11: Token![,],
    #[syn(bracketed)]
    _bracket_token5: syn::token::Bracket,
    #[syn(in = _bracket_token5)]
    implementation: Type,
    _comma_12: Token![,],
    #[syn(bracketed)]
    _bracket_token6: syn::token::Bracket,
    #[syn(in = _bracket_token6)]
    #[parse(Punctuated::parse_terminated)]
    supertraits: Punctuated<Path, Token![,]>,
    _comma_13: Token![,],
    #[syn(bracketed)]
    _bracket_token7: syn::token::Bracket,
    #[syn(in = _bracket_token7)]
    #[parse(Punctuated::parse_terminated)]
    derive_traits: Punctuated<Ident, Token![,]>,
    _comma_14: Token![,],
}

fn has_self_ty(ty: &Type) -> Option<Span> {
    use syn::visit::Visit;
    struct Visitor(Option<Span>);
    impl<'a> syn::visit::Visit<'a> for Visitor {
        fn visit_type(&mut self, i: &Type) {
            match i {
                Type::Path(tp) => {
                    if tp.path.is_ident("Self") {
                        self.0 = Some(tp.path.span());
                        return;
                    }
                }
                _ => (),
            }
            syn::visit::visit_type(self, i)
        }
    }
    let mut visitor = Visitor(None);
    visitor.visit_type(ty);
    visitor.0
}

fn generate_fn_args(
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
                        "Cannot have multiple Self type inputs in sumtrait function signature."
                    )
                } else {
                    recv = Some(ident.clone());
                }
                ret.push((input.clone(), self_arg_id.clone()));
            }
            FnArg::Typed(pt) => {
                let ident = match pt.pat.as_ref() {
                    Pat::Ident(pi) if &pi.ident != "_" => pi.ident.clone(),
                    _ => Ident::new(&format!("__sumtrait_arg_{}", n), pt.span()),
                };
                // Check if supported Self type?
                if pt.ty.as_ref() == &parse_quote!(Self)
                    || pt.ty.as_ref() == &parse_quote!(&Self)
                    || pt.ty.as_ref() == &parse_quote!(&mut Self)
                {
                    if let Some(recv) = &recv {
                        abort!(
                            recv.span().join(pt.ty.span()).unwrap_or(pt.ty.span()),
                            "Cannot have multiple Self type inputs in sumtrait function signature."
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

fn process_fn(
    item_fn: &TraitItemFn,
    typerefs: &HashMap<Type, usize>,
    variants: &Punctuated<IdentAndType, Token![,]>,
    iter_ty_generics: &[GenericArgument],
    enum_path: &Path,
    krate: &Path,
    marker_path: &Path,
    implementation: &Path,
    typeref_id: &LitInt,
) -> TokenStream {
    let self_arg_ident = Ident::new("__sumtrait_self_arg", Span::call_site());
    let (args, recv, return_is_self) = generate_fn_args(&item_fn.sig, &self_arg_ident);
    let (fn_impl_generics, _, fn_where_clause) = item_fn.sig.generics.split_for_impl();
    quote! {
        #{&item_fn.sig.constness} #{&item_fn.sig.asyncness} #{&item_fn.sig.unsafety}
        #{&item_fn.sig.abi} fn #{&item_fn.sig.ident} #fn_impl_generics (
            #(for (input, _) in &args), {
                #(if let FnArg::Typed(pat_type) = input) {
                    #(if let Some(index) = typerefs.get(&pat_type.ty)) {
                        #{&pat_type.pat} #{&pat_type.colon_token} <#marker_path as #krate::TypeRef<#typeref_id, #index>>::Type
                    } #(else) {
                        #pat_type
                    }
                } #(else) {
                    #input
                }
            }
            #{&item_fn.sig.variadic}
        )
        #(if let ReturnType::Type(arr, ty) = &item_fn.sig.output) {
            #arr
            #(if let Some(index) = typerefs.get(&ty)) {
                <#marker_path as #krate::TypeRef<#typeref_id, #index>>::Type
            } #(else) {
                #ty
            }
        } #fn_where_clause {
            #(if let Some(recv) = recv) {
                match #recv {
                    #(for IdentAndType{ident, ty, ..} in variants) {
                        #enum_path::#ident(#self_arg_ident) =>
                            #(if return_is_self) {
                                #enum_path::#ident(<#ty as #implementation<
                                     #(#iter_ty_generics),*
                                     >>::#{&item_fn.sig.ident}(
                                         #(for (_, arg) in &args), { #arg }
                                )),
                            } #(else) {
                                <#ty as #implementation<
                                     #(#iter_ty_generics),*
                                 >>::#{&item_fn.sig.ident}(
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
    a.sort_by(|lhs, rhs| {
        use std::cmp::Ordering;
        if lhs == rhs {
            Ordering::Equal
        } else {
            match (lhs, rhs) {
                (GenericParam::Lifetime(_), _)
                | (GenericParam::Const(_), GenericParam::Type(_)) => Ordering::Less,
                _ => Ordering::Greater,
            }
        }
    });
}

fn arr_to_ts<I: IntoIterator>(a: I) -> impl Iterator<Item = TokenStream>
where
    I::Item: ToTokens,
{
    a.into_iter().map(|a| quote!(#a))
}

pub fn sumtrait_internal(input: TokenStream) -> TokenStream {
    let SumtraitInternalContent {
        inner:
            SumtraitInternalContextInner {
                trait_path,
                enum_path,
                unspecified_ty_params,
                variants,
                impl_generics_base,
                ty_generics_base,
                where_clause_base,
                constraint_expr_trait_ident,
                ..
            },
        typerefs,
        item_trait,
        typeref_id,
        krate,
        marker_path,
        implementation,
        supertraits,
        derive_traits,
        ..
    } = parse2(input.clone())
        .unwrap_or_else(|e| abort!(Span::call_site(), format!("Bad content: {}", e)));

    #[derive(syn_derive::Parse)]
    struct TraitIdentAndOther {
        #[syn(braced)]
        _brace_token0: syn::token::Brace,
        #[syn(in = _brace_token0)]
        _trait_ident: Ident,
        #[syn(in = _brace_token0)]
        _comma_61: Token![,],
        #[syn(in = _brace_token0)]
        other: TokenStream,
        _other2: TokenStream,
    }
    let trait_ident_and_other: TraitIdentAndOther = parse2(input).unwrap();
    let implementation = if let Type::Path(TypePath { path, .. }) = implementation {
        path
    } else {
        trait_path.clone()
    };
    let mut impl_generics_merged = item_trait
        .generics
        .params
        .iter()
        .chain(&impl_generics_base)
        .collect::<Vec<_>>();
    sort_impl_generics(impl_generics_merged.as_mut_slice());
    let impl_generics_base = arr_to_ts(&impl_generics_base).collect::<Vec<_>>();
    let assoc_types = item_trait
        .items
        .iter()
        .filter_map(|item| match item {
            TraitItem::Type(c) => Some((
                Ident::new(
                    &format!("__SumType_AssocType_{}", &c.ident),
                    Span::call_site(),
                ),
                c.clone(),
            )),
            _ => None,
        })
        .collect::<Vec<_>>();
    let (_, ty_generics_trait, _) = split_for_impl(Some(&item_trait.generics));
    let mapped_typerefs: HashMap<Type, usize> = typerefs
        .iter()
        .cloned()
        .enumerate()
        .map(|(l, r)| (r, l))
        .collect();
    let ty_generics_base = ty_generics_base
        .into_iter()
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
    let supertraits_constraint_traits = (0..supertraits.len())
        .map(|n| {
            Ident::new(
                &format!("__SumTrait_ConstraintTrait_{}_{}", n, random()),
                Span::call_site(),
            )
        })
        .collect::<Vec<_>>();
    quote! {
        #(for (supertrait, constraint_trait) in supertraits.iter().zip(&supertraits_constraint_traits)) {
            #supertrait!(
                #constraint_trait,
                #{&trait_ident_and_other.other}
            );
        }
        trait #constraint_expr_trait_ident<#(#impl_generics_base),*> {}
        impl<#(#impl_generics),*> #constraint_expr_trait_ident<#(#ty_generics_base),*>
            for #enum_path <#(#ty_generics_enum),*>
         where
            #where_clause_base
            #(if where_clause_base.to_string().len() > 0) { , }
            #(for IdentAndType{ty, ..} in &variants) {
                #ty: #implementation<
                    #(#ty_generics_trait),*
                    #(if ty_generics_trait.len() > 0 && assoc_types.len() > 0) { , }
                    #(for (atp, at) in &assoc_types),{
                        #{&at.ident} = #atp
                    }
                >,
            }
            #(for constraint_trait in &supertraits_constraint_traits) {
                Self: #constraint_trait<#(#ty_generics_base),*>,
            }
        { }
        impl <#(#impl_generics),*> #implementation<#(for p in &item_trait.generics.params),{#p}>
            for #enum_path<#(#ty_generics_enum),*>
        where
            #where_clause_base
            #(if where_clause_base.to_string().len() > 0) { , }
            #(for IdentAndType{ty, ..} in &variants) {
                #ty: #implementation<
                    #(#ty_generics_trait),*
                    #(if ty_generics_trait.len() > 0 && assoc_types.len() > 0) { , }
                    #(for (atp, at) in &assoc_types), {
                        #{&at.ident} = #atp
                    }
                >,
            }
            #(for supertrait in &supertraits) {
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
                #{process_fn(
                    f,
                    &mapped_typerefs,
                    &variants,
                    &ty_generics_trait,
                    &enum_path,
                    &krate,
                    &marker_path,
                    &implementation,
                    &typeref_id
                )}
            }
        }
    }
}
