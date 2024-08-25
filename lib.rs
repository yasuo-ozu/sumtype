#![doc = include_str!("README.md")]

use derive_syn_parse::Parse;
use proc_macro::TokenStream as TokenStream1;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use proc_macro_error::{abort, proc_macro_error};
use syn::spanned::Spanned;
use syn::*;
use template_quote::{quote, ToTokens};

fn random() -> u64 {
    use std::hash::{BuildHasher, Hasher};
    std::collections::hash_map::RandomState::new()
        .build_hasher()
        .finish()
}

fn path_of_ident(ident: Ident) -> Path {
    Path {
        leading_colon: None,
        segments: std::iter::once(PathSegment {
            ident,
            arguments: PathArguments::None,
        })
        .collect(),
    }
}

fn split_for_impl(
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

#[derive(Parse)]
struct Arguments {}

enum SumTypeImpl {
    Iterator,
}

impl SumTypeImpl {
    fn gen(
        &self,
        enum_path: &Path,
        ty_params: &[Ident],
        variants: &[(Ident, Type)],
        generics: Option<&Generics>,
    ) -> TokenStream {
        let (impl_generics, ty_generics, where_clause) = split_for_impl(generics);
        quote! {
            impl <#(#impl_generics,)* __SumType_Item #(,#ty_params)*> ::core::iter::Iterator for #enum_path<#(#ty_generics,)*#(#ty_params),*>
            where
                #(#where_clause,)*
                #(for (_, ty) in variants) {
                    #ty: ::core::iter::Iterator<Item = __SumType_Item>,
                }
            {
                type Item = __SumType_Item;
                fn next(&mut self) -> Option<Self::Item> {
                    match self {
                        #(for (ident, _) in variants) {
                            Self::#ident(__sumtype_val) => __sumtype_val.next(),
                        }
                        Self::__Uninhabited(_) => ::core::unreachable!(),
                    }
                }
            }
        }
    }
}

// Factory methods to process on supported tree elements
trait ProcessTree: Sized {
    // Collect macros in both type context and expr context. Replace macros with code.
    fn collect_inline_macro(
        &mut self,
        enum_path: &Path,
        typeref_path: &Path,
        generics: Option<&Generics>,
    ) -> (Vec<(Span, Ident, Option<Ident>)>, Vec<Span>);

    fn emit_items(mut self, generics: Option<&Generics>) -> (TokenStream, Self) {
        let r = random();
        let enum_ident = Ident::new(&format!("__Sumtype_Enum_{}", r), Span::call_site());
        let typeref_ident =
            Ident::new(&format!("__Sumtype_TypeRef_Trait_{}", r), Span::call_site());
        let (found_exprs, type_emitted) = self.collect_inline_macro(
            &path_of_ident(enum_ident.clone()),
            &path_of_ident(typeref_ident.clone()),
            generics,
        );
        let reftypes = found_exprs
            .iter()
            .filter_map(|(_, _, reft)| reft.clone())
            .collect::<Vec<_>>();
        let (impl_generics, ty_generics, where_clause) = split_for_impl(generics);
        let (ty_params, variants) = found_exprs.iter().enumerate().fold(
            (vec![], vec![]),
            |(mut ty_params, mut variants), (i, (_, ident, reft))| {
                if let Some(reft) = reft {
                    variants.push((
                        ident.clone(),
                        parse_quote!(<#reft as #typeref_ident<#(#ty_generics),*>>::Type),
                    ));
                } else {
                    let tp_ident =
                        Ident::new(&format!("__Sumtype_TypeParam_{}", i), Span::call_site());
                    variants.push((ident.clone(), parse_quote!(#tp_ident)));
                    ty_params.push(tp_ident);
                }
                (ty_params, variants)
            },
        );
        if let (Some((span, _, _)), true) = (
            found_exprs
                .iter()
                .filter(|(_, _, reft)| reft.is_none())
                .next(),
            type_emitted.len() > 0,
        ) {
            abort!(
                span,
                r#"
To emit full type, you should specify the type.
Example: sumtype!(std::iter::empty(), std::iter::Empty<T>)
"#
            )
        } else {
            let replaced_ty_generics: Vec<_> = ty_generics
                .iter()
                .map(|ga| match ga {
                    GenericArgument::Lifetime(lt) => quote!(& #lt ()),
                    GenericArgument::Const(_) => quote!(),
                    o => quote!(#o),
                })
                .collect();
            let out = quote! {
                #(for reft in &reftypes) {
                    struct #reft;
                }
                trait #typeref_ident <#(#impl_generics),*> { type Type; }
                enum #enum_ident <#(#impl_generics,)*#(#ty_params),*> {
                    #(for (ident, ty) in &variants) {
                        #ident ( #ty ),
                    }
                    __Uninhabited(
                        (
                            ::core::convert::Infallible,
                            #(::core::marker::PhantomData<#replaced_ty_generics>),*
                        )
                    ),
                }
                impl<#(#impl_generics,)* __SumType_Item #(,#ty_params)*> #typeref_ident <#(#ty_generics),*>
                    for #enum_ident <#(#ty_generics,)*#(#ty_params),*>
                where
                    #(#where_clause,)*
                    #(for (_, ty) in &variants) {
                        #ty: ::core::iter::Iterator<Item = __SumType_Item>,
                    }
                {
                    type Type = __SumType_Item;
                }
                #{ SumTypeImpl::Iterator.gen(
                    &path_of_ident(enum_ident),
                    ty_params.as_slice(),
                    variants.as_slice(),
                    generics
                ) }
            };
            (out, self)
        }
    }
}

const _: () = {
    use syn::visit_mut::VisitMut;
    struct Visitor<'a> {
        enum_path: &'a Path,
        typeref_path: &'a Path,
        found_exprs: Vec<(Span, Ident, Option<Ident>)>,
        emit_type: Vec<Span>,
        generics: Option<&'a Generics>,
    }

    impl ProcessTree for Block {
        fn collect_inline_macro(
            &mut self,
            enum_path: &Path,
            typeref_path: &Path,
            generics: Option<&Generics>,
        ) -> (Vec<(Span, Ident, Option<Ident>)>, Vec<Span>) {
            let mut visitor = Visitor::new(enum_path, typeref_path, generics);
            visitor.visit_block_mut(self);
            (visitor.found_exprs, visitor.emit_type)
        }
    }

    impl ProcessTree for Item {
        fn collect_inline_macro(
            &mut self,
            enum_path: &Path,
            typeref_path: &Path,
            generics: Option<&Generics>,
        ) -> (Vec<(Span, Ident, Option<Ident>)>, Vec<Span>) {
            let mut visitor = Visitor::new(enum_path, typeref_path, generics);
            visitor.visit_item_mut(self);
            (visitor.found_exprs, visitor.emit_type)
        }
    }

    impl ProcessTree for Stmt {
        fn collect_inline_macro(
            &mut self,
            enum_path: &Path,
            typeref_path: &Path,
            generics: Option<&Generics>,
        ) -> (Vec<(Span, Ident, Option<Ident>)>, Vec<Span>) {
            let mut visitor = Visitor::new(enum_path, typeref_path, generics);
            visitor.visit_stmt_mut(self);
            (visitor.found_exprs, visitor.emit_type)
        }
    }

    impl<'a> Visitor<'a> {
        fn new(
            enum_path: &'a Path,
            typeref_path: &'a Path,
            generics: Option<&'a Generics>,
        ) -> Self {
            Self {
                enum_path,
                typeref_path,
                found_exprs: Vec::new(),
                emit_type: Vec::new(),
                generics,
            }
        }
        fn do_type_macro(&mut self, mac: &Macro) -> TokenStream {
            #[derive(Parse)]
            struct Arg();
            let _: Arg = mac
                .parse_body()
                .unwrap_or_else(|e| abort!(e.span(), &format!("{}", &e)));
            self.emit_type.push(mac.span());
            let ty_generics = self.generics.map(|g| g.split_for_impl().1);
            quote! { #{&self.enum_path} #ty_generics }
        }

        fn do_expr_macro(&mut self, mac: &Macro) -> TokenStream {
            #[derive(Parse)]
            struct Arg {
                expr: Expr,
                _comma_token: Option<Token![,]>,
                #[parse_if(_comma_token.is_some())]
                ty: Option<Type>,
            }
            let arg: Arg = mac
                .parse_body()
                .unwrap_or_else(|e| abort!(e.span(), &format!("{}", &e)));
            let n = self.found_exprs.len();
            let variant_ident = Ident::new(&format!("__SumType_Variant_{}", n), Span::call_site());
            let reftype_ident = Ident::new(
                &format!("__SumType_RefType_{}_{}", random(), n),
                Span::call_site(),
            );
            let id_fn_ident =
                Ident::new(&format!("__sum_type_id_fn_{}", random()), Span::call_site());
            let (impl_generics, ty_generics, where_clause) = split_for_impl(self.generics);
            self.found_exprs.push((
                mac.span(),
                variant_ident.clone(),
                arg.ty.as_ref().map(|_| reftype_ident.clone()),
            ));
            quote! {
                {
                    #(if let Some(ty) = &arg.ty){
                        impl<#(#impl_generics,)*> #{&self.typeref_path} <#(#ty_generics),*> for #reftype_ident
                            #(if where_clause.len() > 0) {
                                where #(#where_clause,)*
                            }
                        {
                            type Type = #ty;
                        }
                    }
                    fn #id_fn_ident<#(#impl_generics,)*__SumType_T: #{&self.typeref_path} <#(#ty_generics),*>>(t: __SumType_T) -> __SumType_T { t }
                    #id_fn_ident::<#(#ty_generics,)* _>(#{&self.enum_path}::#variant_ident(#{&arg.expr}))
                }
            }
        }
    }

    impl<'a> VisitMut for Visitor<'a> {
        fn visit_type_mut(&mut self, ty: &mut Type) {
            if let Type::Macro(tm) = &*ty {
                let out = self.do_type_macro(&tm.mac);
                *ty = parse2(out).unwrap();
            } else {
                syn::visit_mut::visit_type_mut(self, ty);
            }
        }

        fn visit_expr_mut(&mut self, expr: &mut Expr) {
            if let Expr::Macro(em) = &*expr {
                let out = self.do_expr_macro(&em.mac);
                *expr = parse2(out).unwrap();
            } else {
                syn::visit_mut::visit_expr_mut(self, expr);
            }
        }

        fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
            if let Stmt::Macro(sm) = &*stmt {
                let out = self.do_expr_macro(&sm.mac);
                *stmt = parse2(out).unwrap();
            } else {
                syn::visit_mut::visit_stmt_mut(self, stmt);
            }
        }
    }
};

fn inner(_args: Arguments, input: TokenStream) -> TokenStream {
    if let Ok(block) = parse2::<Block>(input.clone()) {
        let (out, block) = block.emit_items(None);
        quote! { #out #block }
    } else if let Ok(item_enum) = parse2::<ItemEnum>(input.clone()) {
        let generics = item_enum.generics.clone();
        let (out, item) = Item::Enum(item_enum).emit_items(Some(&generics));
        quote! { #out #item }
    } else if let Ok(item_struct) = parse2::<ItemStruct>(input.clone()) {
        let generics = item_struct.generics.clone();
        let (out, item) = Item::Struct(item_struct).emit_items(Some(&generics));
        quote! { #out #item }
    } else if let Ok(item_impl) = parse2::<ItemImpl>(input.clone()) {
        let generics = item_impl.generics.clone();
        let (out, item) = Item::Impl(item_impl).emit_items(Some(&generics));
        quote! { #out #item }
    } else if let Ok(item_union) = parse2::<ItemUnion>(input.clone()) {
        let generics = item_union.generics.clone();
        let (out, item) = Item::Union(item_union).emit_items(Some(&generics));
        quote! { #out #item }
    } else if let Ok(item_fn) = parse2::<ItemFn>(input.clone()) {
        let generics = item_fn.sig.generics.clone();
        let (out, item) = Item::Fn(item_fn).emit_items(Some(&generics));
        quote! { #out #item }
    } else if let Ok(item) = parse2::<Item>(input.clone()) {
        let (out, item) = item.emit_items(None);
        quote! { #out #item }
    } else if let Ok(stmt) = parse2::<Stmt>(input.clone()) {
        let (out, stmt) = stmt.emit_items(None);
        quote! { #out #stmt }
    } else {
        abort!(input.span(), "This elementn is not supported")
    }
}

/// Enabling `sumtype!(..)` macro in the context.
///
/// For each context marked by `#[sumtype]`, sumtype makes an union type of several
/// [`std::iter::Iterator`] types. To intern an expression of `Iterator` into the union type, you
/// can use `sumtype!([expr])` syntax. This is an example of returning unified `Iterator`:
///
/// ```
/// # use sumtype::sumtype;
/// # use std::iter::Iterator;
/// #[sumtype]
/// fn return_iter(a: bool) -> impl Iterator<Item = ()> {
///     if a {
///         sumtype!(std::iter::once(()))
///     } else {
///         sumtype!(vec![()].into_iter())
///     }
/// }
/// ```
///
/// This function returns [`std::iter::Once`] or [`std::vec::IntoIter`] depends on `a` value. The
/// `#[sumtype]` system make annonymous union type that is also [`std::iter::Iterator`], and wrap
/// each `sumtype!(..)` expression with the union type. The mechanism is zerocost when `a` is fixed
/// in the compile process.
///
/// You can place exact (non-annonymous) type using `sumtype!()` macro in type context. In this
/// way, you should specify type using `sumtype!([expr], [type])` format like:
///
/// ```
/// # use sumtype::sumtype;
/// # use std::iter::Iterator;
/// #[sumtype]
/// fn return_iter_explicit(a: bool) -> sumtype!() {
///     if a {
///         sumtype!(std::iter::once(()), std::iter::Once<()>)
///     } else {
///         sumtype!(vec![()].into_iter(), std::vec::IntoIter<()>)
///     }
/// }
/// ```
#[proc_macro_error]
#[proc_macro_attribute]
pub fn sumtype(attr: TokenStream1, input: TokenStream1) -> TokenStream1 {
    inner(parse_macro_input!(attr as Arguments), input.into()).into()
}
