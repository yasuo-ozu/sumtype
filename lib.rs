#![doc = include_str!("README.md")]

use derive_syn_parse::Parse;
use proc_macro::TokenStream as TokenStream1;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use proc_macro_error::{abort, proc_macro_error};
use std::collections::{HashMap, HashSet};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::*;
use template_quote::{quote, ToTokens};

fn random() -> u64 {
    use std::hash::{BuildHasher, Hasher};
    std::collections::hash_map::RandomState::new()
        .build_hasher()
        .finish()
}

fn generic_param_to_arg(i: GenericParam) -> GenericArgument {
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

fn merge_generic_params(
    args1: impl IntoIterator<Item = GenericParam, IntoIter: Clone>,
    args2: impl IntoIterator<Item = GenericParam, IntoIter: Clone>,
) -> impl Iterator<Item = GenericParam> {
    let it1 = args1.into_iter();
    let it2 = args2.into_iter();
    it1.clone()
        .filter(|arg| {
            if let GenericParam::Lifetime(_) = arg {
                true
            } else {
                false
            }
        })
        .chain(it2.clone().filter(|arg| {
            if let GenericParam::Lifetime(_) = arg {
                true
            } else {
                false
            }
        }))
        .chain(it1.clone().filter(|arg| {
            if let GenericParam::Const(_) = arg {
                true
            } else {
                false
            }
        }))
        .chain(it2.clone().filter(|arg| {
            if let GenericParam::Const(_) = arg {
                true
            } else {
                false
            }
        }))
        .chain(it1.clone().filter(|arg| {
            if let GenericParam::Type(_) = arg {
                true
            } else {
                false
            }
        }))
        .chain(it2.clone().filter(|arg| {
            if let GenericParam::Type(_) = arg {
                true
            } else {
                false
            }
        }))
}

fn merge_generic_args(
    args1: impl IntoIterator<Item = GenericArgument, IntoIter: Clone>,
    args2: impl IntoIterator<Item = GenericArgument, IntoIter: Clone>,
) -> impl Iterator<Item = GenericArgument> {
    let it1 = args1.into_iter();
    let it2 = args2.into_iter();
    it1.clone()
        .filter(|arg| {
            if let GenericArgument::Lifetime(_) = arg {
                true
            } else {
                false
            }
        })
        .chain(it2.clone().filter(|arg| {
            if let GenericArgument::Lifetime(_) = arg {
                true
            } else {
                false
            }
        }))
        .chain(it1.clone().filter(|arg| {
            if let GenericArgument::Const(_) = arg {
                true
            } else {
                false
            }
        }))
        .chain(it2.clone().filter(|arg| {
            if let GenericArgument::Const(_) = arg {
                true
            } else {
                false
            }
        }))
        .chain(it1.clone().filter(|arg| {
            if let GenericArgument::Type(_) = arg {
                true
            } else {
                false
            }
        }))
        .chain(it2.clone().filter(|arg| {
            if let GenericArgument::Type(_) = arg {
                true
            } else {
                false
            }
        }))
        .chain(it1.filter(|arg| match arg {
            GenericArgument::AssocType(_)
            | GenericArgument::AssocConst(_)
            | GenericArgument::Constraint(_) => true,
            _ => false,
        }))
        .chain(it2.filter(|arg| match arg {
            GenericArgument::AssocType(_)
            | GenericArgument::AssocConst(_)
            | GenericArgument::Constraint(_) => true,
            _ => false,
        }))
}

fn path_of_ident(ident: Ident, is_super: bool) -> Path {
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
        impl_generics: Vec<GenericParam>,
        ty_generics: Vec<GenericArgument>,
        where_clause: Vec<WherePredicate>,
    ) -> TokenStream {
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
        is_module: bool,
    ) -> (
        Vec<(
            Span,
            Ident,
            Option<Ident>,
            HashMap<Ident, HashSet<Lifetime>>,
            Option<Punctuated<GenericParam, Token![,]>>,
        )>,
        Vec<(Span, Punctuated<GenericArgument, Token![,]>)>,
    );

    fn emit_items(
        mut self,
        generics: Option<&Generics>,
        is_module: bool,
        vis: Visibility,
    ) -> (TokenStream, Self) {
        let r = random();
        let enum_ident = Ident::new(&format!("__Sumtype_Enum_{}", r), Span::call_site());
        let typeref_ident =
            Ident::new(&format!("__Sumtype_TypeRef_Trait_{}", r), Span::call_site());
        let (found_exprs, type_emitted) = self.collect_inline_macro(
            &path_of_ident(enum_ident.clone(), is_module),
            &path_of_ident(typeref_ident.clone(), is_module),
            generics,
            is_module,
        );
        let reftypes = found_exprs
            .iter()
            .filter_map(|(_, _, reft, _, _)| reft.clone())
            .collect::<Vec<_>>();
        let (mut impl_generics, _, where_clause) = split_for_impl(generics);
        if found_exprs.len() == 0 {
            abort!(Span::call_site(), "Cannot find any sumtype!() in expr");
        }
        let analyzed = found_exprs
            .iter()
            .fold(HashMap::new(), |mut acc, (_, _, _, ana, _)| {
                for (id, lts) in ana {
                    acc.entry(id.clone()).or_insert(HashSet::new()).extend(lts)
                }
                acc
            });
        let expr_gparams = found_exprs.iter().fold(HashMap::new(), |mut acc, item| {
            *acc.entry(item.4.clone()).or_insert(0usize) += 1;
            acc
        });
        if expr_gparams.len() != 1 {
            let mut expr_gparams = expr_gparams.into_iter().collect::<Vec<_>>();
            expr_gparams.sort_by_key(|item| item.1);
            abort!(expr_gparams[0].0.span(), "Generic argument mismatch");
        }
        let expr_gparam = expr_gparams
            .into_iter()
            .next()
            .unwrap()
            .0
            .into_iter()
            .flatten()
            .collect::<Vec<_>>();
        let expr_garg = expr_gparam
            .iter()
            .cloned()
            .map(generic_param_to_arg)
            .collect::<Vec<_>>();
        for (_, gargs) in &type_emitted {
            if gargs.len() != expr_garg.len()
                || !expr_garg.iter().zip(gargs).all(|two| match two {
                    (GenericArgument::Lifetime(_), GenericArgument::Lifetime(_))
                    | (GenericArgument::Const(_), GenericArgument::Const(_))
                    | (GenericArgument::Type(_), GenericArgument::Type(_)) => true,
                    _ => false,
                })
            {
                abort!(
                    gargs.span(),
                    "The generic arguments are incompatible with generic params in expression."
                )
            }
        }
        for g in impl_generics.iter_mut() {
            if let GenericParam::Type(TypeParam { ident, bounds, .. }) = g {
                if let Some(lts) = analyzed.get(ident) {
                    for lt in lts {
                        bounds.push(TypeParamBound::Lifetime((*lt).clone()));
                    }
                }
            }
        }
        let impl_generics = merge_generic_params(impl_generics, expr_gparam).collect::<Vec<_>>();
        let ty_generics = impl_generics
            .iter()
            .cloned()
            .map(generic_param_to_arg)
            .collect::<Vec<_>>();
        let (ty_params, variants) = found_exprs.iter().enumerate().fold(
            (vec![], vec![]),
            |(mut ty_params, mut variants), (i, (_, ident, reft, _, _))| {
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
        if let (Some((span, _, _, _, _)), true) = (
            found_exprs
                .iter()
                .filter(|(_, _, reft, _, _)| reft.is_none())
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
                #[derive(Clone, Debug)]
                #vis enum #enum_ident <#(#impl_generics,)*#(#ty_params),*> {
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
                    &path_of_ident(enum_ident, false),
                    ty_params.as_slice(),
                    variants.as_slice(),
                    impl_generics,
                    ty_generics,
                    where_clause
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
        found_exprs: Vec<(
            Span,
            Ident,
            Option<Ident>,
            HashMap<Ident, HashSet<Lifetime>>,
            Option<Punctuated<GenericParam, Token![,]>>,
        )>,
        emit_type: Vec<(Span, Punctuated<GenericArgument, Token![,]>)>,
        generics: Option<&'a Generics>,
        is_module: bool,
    }

    impl ProcessTree for Block {
        fn collect_inline_macro(
            &mut self,
            enum_path: &Path,
            typeref_path: &Path,
            generics: Option<&Generics>,
            is_module: bool,
        ) -> (
            Vec<(
                Span,
                Ident,
                Option<Ident>,
                HashMap<Ident, HashSet<Lifetime>>,
                Option<Punctuated<GenericParam, Token![,]>>,
            )>,
            Vec<(Span, Punctuated<GenericArgument, Token![,]>)>,
        ) {
            let mut visitor = Visitor::new(enum_path, typeref_path, generics, is_module);
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
            is_module: bool,
        ) -> (
            Vec<(
                Span,
                Ident,
                Option<Ident>,
                HashMap<Ident, HashSet<Lifetime>>,
                Option<Punctuated<GenericParam, Token![,]>>,
            )>,
            Vec<(Span, Punctuated<GenericArgument, Token![,]>)>,
        ) {
            let mut visitor = Visitor::new(enum_path, typeref_path, generics, is_module);
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
            is_module: bool,
        ) -> (
            Vec<(
                Span,
                Ident,
                Option<Ident>,
                HashMap<Ident, HashSet<Lifetime>>,
                Option<Punctuated<GenericParam, Token![,]>>,
            )>,
            Vec<(Span, Punctuated<GenericArgument, Token![,]>)>,
        ) {
            let mut visitor = Visitor::new(enum_path, typeref_path, generics, is_module);
            visitor.visit_stmt_mut(self);
            (visitor.found_exprs, visitor.emit_type)
        }
    }

    impl<'a> Visitor<'a> {
        fn new(
            enum_path: &'a Path,
            typeref_path: &'a Path,
            generics: Option<&'a Generics>,
            is_module: bool,
        ) -> Self {
            Self {
                enum_path,
                typeref_path,
                found_exprs: Vec::new(),
                emit_type: Vec::new(),
                generics,
                is_module,
            }
        }
        fn do_type_macro(&mut self, mac: &Macro) -> TokenStream {
            #[derive(Parse)]
            struct Arg {
                #[call(Punctuated::parse_terminated)]
                generic_args: Punctuated<GenericArgument, Token![,]>,
            }
            let arg: Arg = mac
                .parse_body()
                .unwrap_or_else(|e| abort!(e.span(), &format!("{}", &e)));
            let ty_generics = merge_generic_args(
                self.generics
                    .iter()
                    .map(|g| g.params.iter().cloned().map(generic_param_to_arg))
                    .flatten(),
                arg.generic_args.clone(),
            )
            .collect::<Vec<_>>();
            self.emit_type.push((mac.span(), arg.generic_args));
            quote! {
                #{&self.enum_path}
                #(if ty_generics.len() > 0){
                    <#(#ty_generics),*>
                }
            }
        }

        fn analyze_lifetime_bounds(
            &self,
            generics: &Generics,
            ty: &Type,
        ) -> HashMap<Ident, HashSet<Lifetime>> {
            struct LifetimeVisitor {
                generic_lifetimes: HashSet<Lifetime>,
                generic_params: HashSet<Ident>,
                lifetime_stack: Vec<Lifetime>,
                result: HashMap<Ident, HashSet<Lifetime>>,
            }
            use syn::visit::Visit;
            impl<'ast> syn::visit::Visit<'ast> for LifetimeVisitor {
                fn visit_type_reference(&mut self, i: &TypeReference) {
                    if let Some(lt) = &i.lifetime {
                        if self.generic_lifetimes.contains(lt) {
                            self.lifetime_stack.push(lt.clone());
                            syn::visit::visit_type_reference(self, i);
                            self.lifetime_stack.pop();
                            return;
                        }
                    }
                    syn::visit::visit_type_reference(self, i);
                }
                fn visit_type_path(&mut self, i: &TypePath) {
                    if i.qself.is_none() {
                        if let Some(id) = i.path.get_ident() {
                            if self.generic_params.contains(id) {
                                self.result
                                    .entry(id.clone())
                                    .or_insert(HashSet::new())
                                    .extend(self.lifetime_stack.clone());
                            }
                            return;
                        }
                    }
                    syn::visit::visit_type_path(self, i);
                }
            }
            let mut visitor = LifetimeVisitor {
                generic_lifetimes: generics
                    .params
                    .iter()
                    .filter_map(|p| {
                        if let GenericParam::Lifetime(LifetimeParam { lifetime, .. }) = p {
                            Some(lifetime.clone())
                        } else {
                            None
                        }
                    })
                    .collect(),
                generic_params: generics
                    .params
                    .iter()
                    .filter_map(|p| {
                        if let GenericParam::Type(TypeParam { ident, .. }) = p {
                            Some(ident.clone())
                        } else {
                            None
                        }
                    })
                    .collect(),
                lifetime_stack: Vec::new(),
                result: HashMap::new(),
            };
            visitor.visit_type(ty);
            visitor.result
        }

        fn do_expr_macro(&mut self, mac: &Macro) -> TokenStream {
            #[derive(Parse)]
            struct Arg {
                expr: Expr,
                _comma_token: Option<Token![,]>,
                _for_token: Option<Token![for]>,
                #[prefix(Option<Token![<]>)]
                #[postfix(Option<Token![>]>)]
                #[parse_if(_for_token.is_some())]
                #[call(Punctuated::parse_separated_nonempty)]
                for_generics: Option<Punctuated<GenericParam, Token![,]>>,
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
            let reftype_path = path_of_ident(reftype_ident.clone(), self.is_module);
            let id_fn_ident =
                Ident::new(&format!("__sum_type_id_fn_{}", random()), Span::call_site());
            let (mut impl_generics, _, where_clause) = split_for_impl(self.generics);
            let analyzed =
                if let (Some(generics), Some(ty)) = (self.generics.as_ref(), arg.ty.as_ref()) {
                    self.analyze_lifetime_bounds(*generics, ty)
                } else {
                    HashMap::new()
                };
            self.found_exprs.push((
                mac.span(),
                variant_ident.clone(),
                arg.ty.as_ref().map(|_| reftype_ident.clone()),
                analyzed.clone(),
                arg.for_generics.clone(),
            ));
            for g in impl_generics.iter_mut() {
                if let GenericParam::Type(TypeParam { ident, bounds, .. }) = g {
                    if let Some(lts) = analyzed.get(ident) {
                        for lt in lts {
                            bounds.push(TypeParamBound::Lifetime(lt.clone().clone()));
                        }
                    }
                }
            }
            let impl_generics =
                merge_generic_params(impl_generics, arg.for_generics.into_iter().flatten())
                    .collect::<Vec<_>>();
            let ty_generics = impl_generics
                .iter()
                .cloned()
                .map(generic_param_to_arg)
                .collect::<Vec<_>>();
            quote! {
                {
                    #(if let Some(ty) = &arg.ty){
                        impl<#(#impl_generics,)*> #{&self.typeref_path} <#(#ty_generics),*> for #reftype_path
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
                if tm.mac.path.is_ident("sumtype") {
                    let out = self.do_type_macro(&tm.mac);
                    *ty = parse2(out).unwrap();
                    return;
                }
            }
            syn::visit_mut::visit_type_mut(self, ty);
        }

        fn visit_expr_mut(&mut self, expr: &mut Expr) {
            if let Expr::Macro(em) = &*expr {
                if em.mac.path.is_ident("sumtype") {
                    let out = self.do_expr_macro(&em.mac);
                    *expr = parse2(out).unwrap();
                    return;
                }
            }
            syn::visit_mut::visit_expr_mut(self, expr);
        }

        fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
            if let Stmt::Macro(sm) = &*stmt {
                if sm.mac.path.is_ident("sumtype") {
                    let out = self.do_expr_macro(&sm.mac);
                    *stmt = parse2(out).unwrap();
                    return;
                }
            }
            syn::visit_mut::visit_stmt_mut(self, stmt);
        }
    }
};

fn inner(_args: Arguments, input: TokenStream) -> TokenStream {
    if let Ok(block) = parse2::<Block>(input.clone()) {
        let (out, block) = block.emit_items(None, false, Visibility::Inherited);
        quote! { #out #block }
    } else if let Ok(item_trait) = parse2::<ItemTrait>(input.clone()) {
        let generics = item_trait.generics.clone();
        let vis = item_trait.vis.clone();
        let (out, item) = Item::Trait(item_trait).emit_items(Some(&generics), false, vis);
        quote! { #out #item }
    } else if let Ok(item_impl) = parse2::<ItemImpl>(input.clone()) {
        let generics = item_impl.generics.clone();
        let (out, item) =
            Item::Impl(item_impl).emit_items(Some(&generics), false, Visibility::Inherited);
        quote! { #out #item }
    } else if let Ok(item_fn) = parse2::<ItemFn>(input.clone()) {
        let generics = item_fn.sig.generics.clone();
        let vis = item_fn.vis.clone();
        let (out, item) = Item::Fn(item_fn).emit_items(Some(&generics), false, vis);
        quote! { #out #item }
    } else if let Ok(item_mod) = parse2::<ItemMod>(input.clone()) {
        let (out, item) = Item::Mod(item_mod).emit_items(None, true, Visibility::Inherited);
        quote! { #out #item }
    } else if let Ok(item) = parse2::<Item>(input.clone()) {
        let (out, item) = item.emit_items(None, false, Visibility::Inherited);
        quote! { #out #item }
    } else if let Ok(stmt) = parse2::<Stmt>(input.clone()) {
        let (out, stmt) = stmt.emit_items(None, false, Visibility::Inherited);
        quote! { #out #stmt }
    } else {
        abort!(input.span(), "This element is not supported")
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
///
/// You may need to use additional generic parameters in `[ty]`, the following example will be
/// useful:
///
/// ```
/// # use sumtype::sumtype;
/// # use std::iter::Iterator;
/// struct S;
///
/// #[sumtype]
/// impl S {
///     fn f<'a, T>(t: &'a T, count: usize) -> sumtype!['a, T] {
///         if count == 0 {
///             sumtype!(std::iter::empty(), for<'a, T: 'a> std::iter::Empty<&'a T>)
///         } else {
///             sumtype!(
///                 std::iter::repeat(t).take(count),
///                 for<'a, T: 'a> std::iter::Take<std::iter::Repeat<&'a T>>
///             )
///         }
///     }
/// }
/// ```
#[proc_macro_error]
#[proc_macro_attribute]
pub fn sumtype(attr: TokenStream1, input: TokenStream1) -> TokenStream1 {
    inner(parse_macro_input!(attr as Arguments), input.into()).into()
}
