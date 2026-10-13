use derive_syn_parse::Parse;
use proc_macro::TokenStream as TokenStream1;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use proc_macro_error::{abort, proc_macro_error};
use std::collections::{HashMap, HashSet};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::*;
use template_quote::quote;

mod generics;
mod sumtype_internal;
mod supertrait;
use generics::*;

/// Resolves the path to the `sumtype` crate, honoring a rename in the downstream `Cargo.toml`
/// (e.g. `renamed = { package = "sumtype" }`). Falls back to `::sumtype` (which also covers the
/// `FoundCrate::Itself` case, where `::sumtype` resolves in the crate's own integration tests).
fn sumtype_crate() -> Path {
    match proc_macro_crate::crate_name("sumtype") {
        Ok(proc_macro_crate::FoundCrate::Name(name)) => {
            let id = Ident::new(&name, Span::call_site());
            parse_quote!(::#id)
        }
        _ => parse_quote!(::sumtype),
    }
}

#[derive(Parse)]
struct Arguments {
    #[call(Punctuated::parse_terminated)]
    bounds: Punctuated<Path, Token![+]>,
}

/// Start the announce trampoline that fetches every `#[sumtype(A + B + ..)]` bound's definition:
/// `A! { [A!, B!, .., krate::_sumtype_internal!] { context } }`. Each trait macro prepends its
/// entry and calls the next; `_sumtype_internal` then receives all definitions at once (and
/// fetches supertraits with a further trampoline round when needed).
#[allow(clippy::too_many_arguments)]
fn gen_trait_impls(
    krate: &Path,
    bounds: &[(&Path, &Ident)],
    enum_path: &Path,
    unspecified_ty_params: &[Ident],
    variants: &[(Ident, Type)],
    impl_generics: &[GenericParam],
    ty_generics: &[GenericArgument],
    where_clause: &[WherePredicate],
) -> TokenStream {
    let list: announce::helper::ContList = bounds
        .iter()
        .map(|(p, _)| announce::helper::Cont::bang((*p).clone()))
        .chain(std::iter::once(announce::helper::Cont::bang(
            parse_quote!(#krate::_sumtype_internal),
        )))
        .collect();
    list.invoke(quote! {
        {
            krate = #krate,
            bounds = [#(for (p, ct) in bounds),{ #p => #ct }],
            enum_path = #enum_path,
            unspecified_ty_params = [#(#unspecified_ty_params),*],
            variants = [#(for (id, ty) in variants),{#id:#ty}],
            impl_generics_base = [ #(#impl_generics),* ],
            ty_generics_base = [#(#ty_generics),*],
            where_clause_base = [#(#where_clause),*],
        }
    })
}

struct ExprMacroInfo {
    span: Span,
    variant_ident: Ident,
    reftype_ident: Option<Ident>,
    analyzed_bounds: HashMap<Ident, HashSet<Lifetime>>,
    generics: Generics,
}

struct TypeMacroInfo {
    _span: Span,
    generic_args: Punctuated<GenericArgument, Token![,]>,
}

// Factory methods to process on supported tree elements
trait ProcessTree: Sized {
    // Collect macros in both type context and expr context. Replace macros with code.
    fn collect_inline_macro(
        &mut self,
        enum_path: &Path,
        typeref_path: &Path,
        constraint_expr_trait_path: &Path,
        generics: Option<&Generics>,
        is_module: bool,
    ) -> (Vec<ExprMacroInfo>, Vec<TypeMacroInfo>);

    fn emit_items(
        mut self,
        args: &Arguments,
        generics_env: Option<&Generics>,
        is_module: bool,
        vis: Visibility,
    ) -> (TokenStream, Self) {
        let r = random();
        let enum_ident = Ident::new(&format!("__Sumtype_Enum_{}", r), Span::call_site());
        let typeref_ident =
            Ident::new(&format!("__Sumtype_TypeRef_Trait_{}", r), Span::call_site());
        let constraint_expr_trait_ident = Ident::new(
            &format!("__Sumtype_ConstraintExprTrait_{}", r),
            Span::call_site(),
        );
        let (found_exprs, type_emitted) = self.collect_inline_macro(
            &path_of_ident(enum_ident.clone(), is_module),
            &path_of_ident(typeref_ident.clone(), is_module),
            &path_of_ident(constraint_expr_trait_ident.clone(), is_module),
            generics_env,
            is_module,
        );
        let reftypes = found_exprs
            .iter()
            .filter_map(|info| info.reftype_ident.clone())
            .collect::<Vec<_>>();
        let (impl_generics_env, _, where_clause_env) = split_for_impl(generics_env);
        if found_exprs.is_empty() {
            abort!(Span::call_site(), "Cannot find any sumtype!() in expr");
        }
        let expr_generics_list = found_exprs.iter().fold(HashMap::new(), |mut acc, info| {
            *acc.entry(info.generics.clone()).or_insert(0usize) += 1;
            acc
        });
        if expr_generics_list.len() != 1 {
            let mut expr_gparams = expr_generics_list.into_iter().collect::<Vec<_>>();
            expr_gparams.sort_by_key(|item| item.1);
            abort!(expr_gparams[0].0.span(), "Generic argument mismatch");
        }
        let expr_generics = expr_generics_list.into_iter().next().unwrap().0;
        let mut analyzed = found_exprs.iter().fold(
            HashMap::new(),
            |mut acc: HashMap<Ident, HashSet<TypeParamBound>>, info| {
                for (id, lts) in &info.analyzed_bounds {
                    acc.entry(id.clone())
                        .or_default()
                        .extend(lts.iter().map(|lt| TypeParamBound::Lifetime(lt.clone())));
                }
                acc
            },
        );
        if let Some(where_clause) = &expr_generics.where_clause {
            for pred in &where_clause.predicates {
                if let WherePredicate::Type(PredicateType {
                    bounded_ty: Type::Path(path),
                    bounds,
                    ..
                }) = pred
                {
                    if path.qself.is_none() {
                        if let Some(id) = path.path.get_ident() {
                            analyzed
                                .entry(id.clone())
                                .or_insert(HashSet::new())
                                .extend(bounds.clone());
                        }
                    }
                }
            }
        }
        let expr_garg = expr_generics
            .params
            .iter()
            .cloned()
            .map(generic_param_to_arg)
            .collect::<Vec<_>>();
        for info in &type_emitted {
            if info.generic_args.len() != expr_garg.len()
                || !expr_garg.iter().zip(&info.generic_args).all(|two| {
                    matches!(
                        two,
                        (GenericArgument::Lifetime(_), GenericArgument::Lifetime(_))
                            | (GenericArgument::Const(_), GenericArgument::Const(_))
                            | (GenericArgument::Type(_), GenericArgument::Type(_))
                    )
                })
            {
                abort!(
                    info.generic_args.span(),
                    "The generic arguments are incompatible with generic params in expression."
                )
            }
        }
        let mut impl_generics =
            merge_generic_params(impl_generics_env, expr_generics.params).collect::<Vec<_>>();
        for g in impl_generics.iter_mut() {
            if let GenericParam::Type(TypeParam { ident, bounds, .. }) = g {
                if let Some(bs) = analyzed.get(ident) {
                    for b in bs {
                        bounds.push(b.clone());
                    }
                }
            }
        }
        let ty_generics = impl_generics
            .iter()
            .cloned()
            .map(generic_param_to_arg)
            .collect::<Vec<_>>();
        let where_clause = expr_generics
            .where_clause
            .clone()
            .map(|wc| wc.predicates)
            .into_iter()
            .flatten()
            .chain(where_clause_env)
            .collect::<Vec<_>>();
        let (unspecified_ty_params, variants) = found_exprs.iter().enumerate().fold(
            (vec![], vec![]),
            |(mut ty_params, mut variants), (i, info)| {
                if let Some(reft) = &info.reftype_ident {
                    variants.push((
                        info.variant_ident.clone(),
                        parse_quote!(<#reft as #typeref_ident<#(#ty_generics),*>>::Type),
                    ));
                } else {
                    let tp_ident =
                        Ident::new(&format!("__Sumtype_TypeParam_{}", i), Span::call_site());
                    variants.push((info.variant_ident.clone(), parse_quote!(#tp_ident)));
                    ty_params.push(tp_ident);
                }
                (ty_params, variants)
            },
        );
        if let (Some(info), true) = (
            found_exprs.iter().find(|info| info.reftype_ident.is_none()),
            !type_emitted.is_empty(),
        ) {
            abort!(
                &info.span,
                r#"
To emit full type, you should specify the type.
Example: sumtype!(std::iter::empty(), std::iter::Empty<T>)
"#
            )
        } else {
            let replaced_ty_generics: Vec<_> = ty_generics
                .iter()
                .filter_map(|ga| match ga {
                    GenericArgument::Lifetime(lt) => Some(quote!(& #lt ())),
                    // Const params need no PhantomData marker; emitting one would produce the
                    // invalid `PhantomData<>` (E0107). Regression test: `tests/fixed_bugs.rs`.
                    GenericArgument::Const(_) => None,
                    o => Some(quote!(#o)),
                })
                .collect();
            let constraint_traits = (0..args.bounds.len())
                .map(|n| {
                    Ident::new(
                        &format!("__Sumtype_ConstraintExprTrait_{}_{}", n, random()),
                        Span::call_site(),
                    )
                })
                .collect::<Vec<_>>();
            // Path to the `sumtype` crate, robust to a rename in the downstream `Cargo.toml`.
            let krate = sumtype_crate();
            let out = quote! {
                #(for reft in &reftypes) {
                    #[doc(hidden)]
                    #[allow(non_camel_case_types)]
                    #[allow(non_camel_case_types)]
                    struct #reft;
                }
                #[doc(hidden)]
                #[allow(non_camel_case_types)]
                trait #typeref_ident <#(#impl_generics),*> { type Type; }
                #[doc(hidden)]
                #[allow(non_camel_case_types)]
                #vis enum #enum_ident <
                    #(#impl_generics),*
                    #(if !impl_generics.is_empty() && !unspecified_ty_params.is_empty()) { , }
                    #(#unspecified_ty_params),*
                > {
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
                #[doc(hidden)]
                #[allow(non_camel_case_types)]
                trait #constraint_expr_trait_ident<#(#impl_generics),*> {}
                impl<#(#impl_generics,)*__Sumtype_TypeParam> #constraint_expr_trait_ident<#(#ty_generics),*> for __Sumtype_TypeParam
                where
                    #(for t in &constraint_traits) {
                        __Sumtype_TypeParam: #t<#(#ty_generics),*>,
                    }
                    #(#where_clause,)*
                {}
                #{ gen_trait_impls(
                    &krate,
                    &args.bounds.iter().zip(&constraint_traits).collect::<Vec<_>>(),
                    &path_of_ident(enum_ident.clone(), false),
                    unspecified_ty_params.as_slice(),
                    variants.as_slice(),
                    &impl_generics,
                    &ty_generics,
                    &where_clause,
                ) }
                // `Downcast` lets callers recover a concrete variant type. It is gated on the
                // wrapped types (and the target) being `'static`, so it is simply unavailable for
                // sum types that wrap borrowed values.
                #[doc(hidden)]
                #[allow(non_camel_case_types)]
                impl<
                    #(#impl_generics,)*
                    #(#unspecified_ty_params,)*
                    __Sumtype_Downcast_To: 'static
                > #krate::Downcast<__Sumtype_Downcast_To>
                    for #enum_ident<#(#ty_generics,)* #(#unspecified_ty_params),*>
                where
                    #(#where_clause,)*
                    // `Self: 'static` gates the impl on the whole sum type (hence every wrapped
                    // type) being `'static`, and decomposes into the region bounds the `&_ as &dyn
                    // Any` casts and `TypeId` lookups need. It is simply unsatisfiable — so the impl
                    // is unavailable, not an error — when a variant borrows.
                    Self: 'static,
                {
                    fn downcast_ref(&self) -> ::core::option::Option<&__Sumtype_Downcast_To> {
                        match self {
                            #(for (vid, _ty) in &variants) {
                                #enum_ident::#vid(__sumtype_v) =>
                                    (__sumtype_v as &dyn ::core::any::Any)
                                        .downcast_ref::<__Sumtype_Downcast_To>(),
                            }
                            #enum_ident::__Uninhabited(_) => ::core::option::Option::None,
                        }
                    }
                    fn downcast_mut(&mut self) -> ::core::option::Option<&mut __Sumtype_Downcast_To> {
                        match self {
                            #(for (vid, _ty) in &variants) {
                                #enum_ident::#vid(__sumtype_v) =>
                                    (__sumtype_v as &mut dyn ::core::any::Any)
                                        .downcast_mut::<__Sumtype_Downcast_To>(),
                            }
                            #enum_ident::__Uninhabited(_) => ::core::option::Option::None,
                        }
                    }
                    fn downcast(self) -> ::core::result::Result<__Sumtype_Downcast_To, Self> {
                        match self {
                            #(for (vid, ty) in &variants) {
                                #enum_ident::#vid(__sumtype_v) => {
                                    match (::std::boxed::Box::new(__sumtype_v)
                                        as ::std::boxed::Box<dyn ::core::any::Any>)
                                        .downcast::<__Sumtype_Downcast_To>()
                                    {
                                        ::core::result::Result::Ok(__sumtype_b) =>
                                            ::core::result::Result::Ok(*__sumtype_b),
                                        ::core::result::Result::Err(__sumtype_any) =>
                                            ::core::result::Result::Err(#enum_ident::#vid(
                                                *__sumtype_any.downcast::<#ty>().ok().unwrap(),
                                            )),
                                    }
                                }
                            }
                            #enum_ident::__Uninhabited(_) => ::core::unreachable!(),
                        }
                    }
                }
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
        constraint_expr_trait_path: &'a Path,
        found_exprs: Vec<ExprMacroInfo>,
        emit_type: Vec<TypeMacroInfo>,
        generics: Option<&'a Generics>,
        is_module: bool,
    }

    impl ProcessTree for Block {
        fn collect_inline_macro(
            &mut self,
            enum_path: &Path,
            typeref_path: &Path,
            constraint_expr_trait_path: &Path,
            generics: Option<&Generics>,
            is_module: bool,
        ) -> (Vec<ExprMacroInfo>, Vec<TypeMacroInfo>) {
            let mut visitor = Visitor::new(
                enum_path,
                typeref_path,
                constraint_expr_trait_path,
                generics,
                is_module,
            );
            visitor.visit_block_mut(self);
            (visitor.found_exprs, visitor.emit_type)
        }
    }

    impl ProcessTree for Item {
        fn collect_inline_macro(
            &mut self,
            enum_path: &Path,
            typeref_path: &Path,
            constraint_expr_trait_path: &Path,
            generics: Option<&Generics>,
            is_module: bool,
        ) -> (Vec<ExprMacroInfo>, Vec<TypeMacroInfo>) {
            let mut visitor = Visitor::new(
                enum_path,
                typeref_path,
                constraint_expr_trait_path,
                generics,
                is_module,
            );
            visitor.visit_item_mut(self);
            (visitor.found_exprs, visitor.emit_type)
        }
    }

    impl ProcessTree for Stmt {
        fn collect_inline_macro(
            &mut self,
            enum_path: &Path,
            typeref_path: &Path,
            constraint_expr_trait_path: &Path,
            generics: Option<&Generics>,
            is_module: bool,
        ) -> (Vec<ExprMacroInfo>, Vec<TypeMacroInfo>) {
            let mut visitor = Visitor::new(
                enum_path,
                typeref_path,
                constraint_expr_trait_path,
                generics,
                is_module,
            );
            visitor.visit_stmt_mut(self);
            (visitor.found_exprs, visitor.emit_type)
        }
    }

    impl<'a> Visitor<'a> {
        fn new(
            enum_path: &'a Path,
            typeref_path: &'a Path,
            constraint_expr_trait_path: &'a Path,
            generics: Option<&'a Generics>,
            is_module: bool,
        ) -> Self {
            Self {
                enum_path,
                typeref_path,
                constraint_expr_trait_path,
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
                .unwrap_or_else(|e| abort!(e.span(), &format!("{}", e)));
            let ty_generics = merge_generic_args(
                self.generics
                    .iter()
                    .flat_map(|g| g.params.iter().cloned().map(generic_param_to_arg)),
                arg.generic_args.clone(),
            )
            .collect::<Vec<_>>();
            self.emit_type.push(TypeMacroInfo {
                _span: mac.span(),
                generic_args: arg.generic_args,
            });
            quote! {
                #{&self.enum_path}
                #(if !ty_generics.is_empty()){
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
            impl syn::visit::Visit<'_> for LifetimeVisitor {
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
                                    .or_default()
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
                #[parse_if(_comma_token.is_some())]
                where_clause: Option<Option<WhereClause>>,
            }
            let arg: Arg = mac
                .parse_body()
                .unwrap_or_else(|e| abort!(e.span(), &format!("{}", e)));
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
                    self.analyze_lifetime_bounds(generics, ty)
                } else {
                    HashMap::new()
                };
            let generics = Generics {
                params: arg.for_generics.clone().unwrap_or_default(),
                where_clause: arg.where_clause.unwrap_or(Some(WhereClause {
                    predicates: Punctuated::new(),
                    where_token: Default::default(),
                })),
                ..Default::default()
            };
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
                merge_generic_params(impl_generics, generics.params.clone()).collect::<Vec<_>>();
            let ty_generics = impl_generics
                .iter()
                .cloned()
                .map(generic_param_to_arg)
                .collect::<Vec<_>>();
            let where_clause = generics
                .where_clause
                .clone()
                .map(|wc| wc.predicates)
                .into_iter()
                .flatten()
                .chain(where_clause)
                .collect::<Vec<_>>();
            self.found_exprs.push(ExprMacroInfo {
                span: mac.span(),
                variant_ident: variant_ident.clone(),
                reftype_ident: arg.ty.as_ref().map(|_| reftype_ident.clone()),
                analyzed_bounds: analyzed.clone(),
                generics,
            });
            quote! {
                {
                    #(if let Some(ty) = &arg.ty){
                        impl<#(#impl_generics,)*> #{&self.typeref_path} <#(#ty_generics),*> for #reftype_path
                            #(if !where_clause.is_empty()) {
                                where #(#where_clause,)*
                            }
                        {
                            type Type = #ty;
                        }
                    }
                    fn #id_fn_ident<
                        #(#impl_generics,)* __SumType_T: #{&self.constraint_expr_trait_path}<#(#ty_generics),*>
                    >(t: __SumType_T) -> __SumType_T
                    #(if !where_clause.is_empty()) {
                        where #(#where_clause,)*
                    }
                    { t }
                    #id_fn_ident::<#(#ty_generics,)*_>(#{&self.enum_path}::#variant_ident(#{&arg.expr}))
                }
            }
        }
    }

    impl VisitMut for Visitor<'_> {
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

fn inner(args: &Arguments, input: TokenStream) -> TokenStream {
    if args.bounds.is_empty() {
        abort!(
            Span::call_site(),
            "`#[sumtype]` requires at least one trait bound, e.g. `#[sumtype(::announce_std::iter::Iterator)]`"
        );
    }
    let public = Visibility::Public(Default::default());
    if let Ok(block) = parse2::<Block>(input.clone()) {
        let (out, block) = block.emit_items(args, None, false, public);
        quote! { #out #[allow(non_local_definitions)] #block }
    } else if let Ok(item_trait) = parse2::<ItemTrait>(input.clone()) {
        let generics = item_trait.generics.clone();
        let vis = item_trait.vis.clone();
        let (out, item) = Item::Trait(item_trait).emit_items(args, Some(&generics), false, vis);
        quote! { #out #[allow(non_local_definitions)] #item }
    } else if let Ok(item_impl) = parse2::<ItemImpl>(input.clone()) {
        let generics = item_impl.generics.clone();
        let (out, item) = Item::Impl(item_impl).emit_items(args, Some(&generics), false, public);
        quote! { #out #[allow(non_local_definitions)] #item }
    } else if let Ok(item_fn) = parse2::<ItemFn>(input.clone()) {
        let generics = item_fn.sig.generics.clone();
        let vis = item_fn.vis.clone();
        let (out, item) = Item::Fn(item_fn).emit_items(args, Some(&generics), false, vis);
        quote! { #out #[allow(non_local_definitions)] #item }
    } else if let Ok(item_mod) = parse2::<ItemMod>(input.clone()) {
        let (out, item) = Item::Mod(item_mod).emit_items(args, None, true, public);
        quote! { #out #[allow(non_local_definitions)] #item }
    } else if let Ok(item) = parse2::<Item>(input.clone()) {
        let (out, item) = item.emit_items(args, None, false, public);
        quote! { #out #[allow(non_local_definitions)] #item }
    } else if let Ok(stmt) = parse2::<Stmt>(input.clone()) {
        let (out, stmt) = stmt.emit_items(args, None, false, public);
        quote! { #out #[allow(non_local_definitions)] #stmt }
    } else {
        abort!(input.span(), "This element is not supported")
    }
}

announce::helper::callback! {
    #[doc(hidden)]
    #[proc_macro_error]
    pub fn _sumtype_internal(input) { sumtype_internal::sumtype_internal(input) }
}

/// The implementation of `sumtype::check`: validates a trait at its definition (see the docs of
/// `sumtype::check`). Run by `#[announce(sumtype::check)]` as `check! { (), TRAIT }`.
#[proc_macro_error]
#[proc_macro]
pub fn check(input: TokenStream1) -> TokenStream1 {
    announce::helper::check(input.into(), &[], |c| match &c.item {
        Item::Trait(item_trait) => {
            supertrait::validate_trait(item_trait, &parse_quote!(::sumtype));
            Ok(TokenStream::new())
        }
        other => Err(Error::new(
            // The first token of the item (a single token, so that the position does not depend on
            // whether the compiler can join spans).
            template_quote::ToTokens::to_token_stream(other)
                .into_iter()
                .next()
                .map_or_else(Span::call_site, |t| t.span()),
            "`sumtype::check` applies to a trait definition",
        )),
    })
    .into()
}

/// Enables the `sumtype!(..)` macro within the annotated context (the implementation of the
/// `#[sumtype]` attribute).
///
/// This is an internal proc-macro crate. End users should depend on the
/// [`sumtype`](https://docs.rs/sumtype) crate and use `#[sumtype(<trait paths>)]` from there;
/// see that crate's documentation for usage, runnable examples, and the list of supported traits.
#[proc_macro_error]
#[proc_macro_attribute]
pub fn sumtype(attr: TokenStream1, input: TokenStream1) -> TokenStream1 {
    inner(&parse_macro_input!(attr as Arguments), input.into()).into()
}
