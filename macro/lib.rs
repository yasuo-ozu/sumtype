use darling::ast::NestedMeta;
use darling::FromMeta;
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

mod sumtrait_internal;

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
struct Arguments {
    #[call(Punctuated::parse_terminated)]
    bounds: Punctuated<Path, Token![+]>,
}

enum SumTypeImpl {
    Trait(Path),
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
        constraint_expr_trait_ident: &Ident,
    ) -> TokenStream {
        match self {
            SumTypeImpl::Trait(trait_path) => {
                quote! {
                    #trait_path!(
                        /* constraint_expr_trait_ident = */ #constraint_expr_trait_ident,
                        /* trait_path = */ #trait_path,
                        /* enum_path = */ #enum_path,
                        /* iter_ty_params = */ [#(#ty_params),*],
                        /* variants = */ [#(for (id, ty) in variants),{#id:#ty}],
                        /* enum_impl_generics = */ [ #(#impl_generics),* ],
                        /* enum_ty_generics = */ [#(#ty_generics),*],
                        /* enum_where_clause = */ { #(#where_clause)* },
                    );
                }
            }
        }
    }
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
        generics: Option<&Generics>,
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
            generics,
            is_module,
        );
        let reftypes = found_exprs
            .iter()
            .filter_map(|info| info.reftype_ident.clone())
            .collect::<Vec<_>>();
        let (impl_generics, _, where_clause) = split_for_impl(generics);
        if found_exprs.len() == 0 {
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
        let mut analyzed = found_exprs.iter().fold(HashMap::new(), |mut acc, info| {
            for (id, lts) in &info.analyzed_bounds {
                acc.entry(id.clone())
                    .or_insert(HashSet::new())
                    .extend(lts.iter().map(|lt| TypeParamBound::Lifetime(lt.clone())));
            }
            acc
        });
        if let Some(where_clause) = &expr_generics.where_clause {
            for pred in &where_clause.predicates {
                match pred {
                    WherePredicate::Type(PredicateType {
                        bounded_ty, bounds, ..
                    }) => {
                        if let Type::Path(path) = bounded_ty {
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
                    _ => (),
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
                || !expr_garg
                    .iter()
                    .zip(&info.generic_args)
                    .all(|two| match two {
                        (GenericArgument::Lifetime(_), GenericArgument::Lifetime(_))
                        | (GenericArgument::Const(_), GenericArgument::Const(_))
                        | (GenericArgument::Type(_), GenericArgument::Type(_)) => true,
                        _ => false,
                    })
            {
                abort!(
                    info.generic_args.span(),
                    "The generic arguments are incompatible with generic params in expression."
                )
            }
        }
        let mut impl_generics =
            merge_generic_params(impl_generics, expr_generics.params).collect::<Vec<_>>();
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
            .chain(where_clause)
            .collect::<Vec<_>>();
        let (ty_params, variants) = found_exprs.iter().enumerate().fold(
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
            found_exprs
                .iter()
                .filter(|info| info.reftype_ident.is_none())
                .next(),
            type_emitted.len() > 0,
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
                .map(|ga| match ga {
                    GenericArgument::Lifetime(lt) => quote!(& #lt ()),
                    GenericArgument::Const(_) => quote!(),
                    o => quote!(#o),
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
            let out = quote! {
                #(for reft in &reftypes) {
                    struct #reft;
                }
                trait #typeref_ident <#(#impl_generics),*> { type Type; }
                #vis enum #enum_ident <
                    #(#impl_generics),*
                    #(if impl_generics.len() > 0 && ty_params.len() > 0) { , }
                    #(#ty_params),*
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
                trait #constraint_expr_trait_ident {}
                impl<__Sumtype_TypeParam> #constraint_expr_trait_ident for __Sumtype_TypeParam
                where
                    #(for t in &constraint_traits) {
                        __Sumtype_TypeParam: #t,
                    }
                {}
                #(for (trait_, constraint_trait) in args.bounds.iter().zip(&constraint_traits)) {
                    #{ SumTypeImpl::Trait(trait_.clone()).gen(
                        &path_of_ident(enum_ident.clone(), false),
                        ty_params.as_slice(),
                        variants.as_slice(),
                        impl_generics.clone(),
                        ty_generics.clone(),
                        where_clause.clone(),
                        constraint_trait,
                    ) }
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
                .unwrap_or_else(|e| abort!(e.span(), &format!("{}", &e)));
            let ty_generics = merge_generic_args(
                self.generics
                    .iter()
                    .map(|g| g.params.iter().cloned().map(generic_param_to_arg))
                    .flatten(),
                arg.generic_args.clone(),
            )
            .collect::<Vec<_>>();
            self.emit_type.push(TypeMacroInfo {
                _span: mac.span(),
                generic_args: arg.generic_args,
            });
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
                #[parse_if(_comma_token.is_some())]
                where_clause: Option<Option<WhereClause>>,
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
            let generics = Generics {
                params: arg.for_generics.clone().unwrap_or(Default::default()),
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
                            #(if where_clause.len() > 0) {
                                where #(#where_clause,)*
                            }
                        {
                            type Type = #ty;
                        }
                    }
                    fn #id_fn_ident<__SumType_T: #{&self.constraint_expr_trait_path} >(t: __SumType_T) -> __SumType_T
                    { t }
                    #id_fn_ident::<_>(#{&self.enum_path}::#variant_ident(#{&arg.expr}))
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

fn inner(args: &Arguments, input: TokenStream) -> TokenStream {
    if let Ok(block) = parse2::<Block>(input.clone()) {
        let (out, block) = block.emit_items(args, None, false, Visibility::Inherited);
        quote! { #out #block }
    } else if let Ok(item_trait) = parse2::<ItemTrait>(input.clone()) {
        let generics = item_trait.generics.clone();
        let vis = item_trait.vis.clone();
        let (out, item) = Item::Trait(item_trait).emit_items(args, Some(&generics), false, vis);
        quote! { #out #item }
    } else if let Ok(item_impl) = parse2::<ItemImpl>(input.clone()) {
        let generics = item_impl.generics.clone();
        let (out, item) =
            Item::Impl(item_impl).emit_items(args, Some(&generics), false, Visibility::Inherited);
        quote! { #out #item }
    } else if let Ok(item_fn) = parse2::<ItemFn>(input.clone()) {
        let generics = item_fn.sig.generics.clone();
        let vis = item_fn.vis.clone();
        let (out, item) = Item::Fn(item_fn).emit_items(args, Some(&generics), false, vis);
        quote! { #out #item }
    } else if let Ok(item_mod) = parse2::<ItemMod>(input.clone()) {
        let (out, item) = Item::Mod(item_mod).emit_items(args, None, true, Visibility::Inherited);
        quote! { #out #item }
    } else if let Ok(item) = parse2::<Item>(input.clone()) {
        let (out, item) = item.emit_items(args, None, false, Visibility::Inherited);
        quote! { #out #item }
    } else if let Ok(stmt) = parse2::<Stmt>(input.clone()) {
        let (out, stmt) = stmt.emit_items(args, None, false, Visibility::Inherited);
        quote! { #out #stmt }
    } else {
        abort!(input.span(), "This element is not supported")
    }
}

fn process_supported_supertraits<'a>(
    traits: impl IntoIterator<Item = &'a TypeParamBound>,
    krate: &Path,
) -> (Vec<Path>, Vec<Path>) {
    let mut supertraits = Vec::new();
    let mut derive_traits = Vec::new();
    for tpb in traits.into_iter() {
        if let TypeParamBound::Trait(tb) = tpb {
            if let Some(ident) = tb.path.get_ident() {
                match ident.to_string().as_str() {
                    "Copy" | "Clone" | "Hash" | "Eq" => {
                        supertraits.push(parse_quote!(#krate::traits::#ident))
                    }
                    "PartialEq" => derive_traits.push(parse_quote!(PartialEq)),
                    o if o.starts_with("__SumTrait_Sealed") => (),
                    _ => (),
                }
            } else {
                supertraits.push(tb.path.clone())
            }
        } else {
            abort!(tpb.span(), "Only path is supported");
        }
    }
    (supertraits, derive_traits)
}

fn collect_typeref_types(input: &ItemTrait) -> Vec<Type> {
    fn can_make_typeref_type(ty: &Type, generics: &Generics) -> bool {
        use syn::visit::Visit;
        struct Visitor(bool, Vec<Ident>, Vec<Ident>);
        let generics_param_tys = generics
            .params
            .iter()
            .filter_map(|p| {
                if let GenericParam::Type(TypeParam { ident, .. }) = p {
                    Some(ident.clone())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        let generics_param_vals = generics
            .params
            .iter()
            .filter_map(|p| {
                if let GenericParam::Const(ConstParam { ident, .. }) = p {
                    Some(ident.clone())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        impl<'a> syn::visit::Visit<'a> for Visitor {
            fn visit_type(&mut self, i: &Type) {
                match i {
                    Type::ImplTrait(_) | Type::Verbatim(_) | Type::Infer(_) | Type::Macro(_) => {
                        self.0 = false;
                    }
                    Type::Reference(TypeReference { lifetime, .. }) => {
                        if let Some(lifetime) = lifetime {
                            if &lifetime.ident != "static" {
                                self.0 = false;
                            }
                        } else {
                            self.0 = false;
                        }
                    }
                    Type::Path(tp) => {
                        if tp.qself.is_none() && self.2.iter().any(|ident| tp.path.is_ident(ident))
                            || (tp.path.segments.len() >= 1 && &tp.path.segments[0].ident == "Self")
                        {
                            self.0 = false;
                        }
                    }
                    _ => (),
                }
                syn::visit::visit_type(self, i)
            }
            fn visit_expr(&mut self, i: &Expr) {
                if let Expr::Path(ExprPath { qself, path, .. }) = i {
                    if qself.is_none() && self.1.iter().any(|ident| path.is_ident(ident)) {
                        self.0 = false;
                    }
                }
            }
        }
        let mut visitor = Visitor(true, generics_param_tys, generics_param_vals);
        visitor.visit_type(ty);
        visitor.0
    }
    use syn::visit::Visit;
    struct Visitor(Vec<Type>, Generics);
    impl<'a> syn::visit::Visit<'a> for Visitor {
        fn visit_type(&mut self, i: &Type) {
            if can_make_typeref_type(i, &self.1) {
                self.0.push(i.clone());
            } else {
                syn::visit::visit_type(self, i)
            }
        }
    }
    let mut visitor = Visitor(Vec::new(), input.generics.clone());
    visitor.visit_item_trait(input);
    visitor.0
}

fn sumtrait_impl(
    args: Option<Path>,
    marker_path: &Path,
    krate: &Path,
    input: ItemTrait,
) -> TokenStream {
    let (supertraits, derive_traits) = process_supported_supertraits(&input.supertraits, krate);
    for item in &input.items {
        match item {
            TraitItem::Const(_) => abort!(item.span(), "associated const is not supported"),
            TraitItem::Fn(tfn) => {
                if tfn.sig.inputs.len() == 0 || !matches!(&tfn.sig.inputs[0], FnArg::Receiver(_)) {
                    abort!(tfn.sig.span(), "requires receiver")
                }
            }
            TraitItem::Type(tty) => {
                if tty.default.is_some() {
                    abort!(tty.span(), "associated type defaults is not supported")
                }
                if tty.generics.params.len() > 0 || tty.generics.where_clause.is_some() {
                    abort!(
                        tty.generics.span(),
                        "generalized associated types is not supported"
                    )
                }
            }
            o => abort!(o.span(), "Not supported"),
        }
    }
    let temporary_mac_name =
        Ident::new(&format!("__sumtype_macro_{}", random()), Span::call_site());
    let typeref_types = collect_typeref_types(&input);
    let (_, _, where_clause) = input.generics.split_for_impl();
    let typeref_id = random() as usize;
    let supertraits_constraint_traits = (0..supertraits.len())
        .map(|n| {
            Ident::new(
                &format!("__SumTrait_ConstraintTrait_{}_{}", n, random()),
                Span::call_site(),
            )
        })
        .collect::<Vec<_>>();
    let constraint_trait = Ident::new(
        &format!("__SumTrait_ConstraintTrait_{}", random()),
        Span::call_site(),
    );

    quote! {
        #input
        #(for (i, ty) in typeref_types.iter().enumerate()) {
            impl<#(for p in &input.generics.params),{#p}> #krate::TypeRef<#typeref_id, #i> for #marker_path #where_clause {
                type Type = #ty;
            }
        }

        #[doc(hidden)]
        #[macro_export]
        macro_rules! #temporary_mac_name {
            (
                $constraint_expr_trait_ident:ident,
                $($t:tt)*
            ) => {
                #(for (supertrait, ctr) in supertraits.iter().zip(&supertraits_constraint_traits)) {
                    #supertrait!(#ctr,$($t)*);
                }
                #(if supertraits.len() > 0) {
                    trait $constraint_expr_trait_ident {}
                    impl<__SumTrait_TypeParam> $constraint_expr_trait_ident for __SumTrait_TypeParam
                    where
                        #(for t in &supertraits_constraint_traits) {
                            __SumTrait_TypeParam: #t,
                        }
                        __SumTrait_TypeParam: #constraint_trait,
                    {}
                }
                ::sumtype::_sumtrait_internal!(
                    #(if supertraits.len() > 0) {
                        #constraint_trait,
                    } #(else) {
                        $constraint_expr_trait_ident,
                    }
                    $($t)*
                    /* typerefs= */  [#(#typeref_types),*],
                    /* item_trait= */  {#input},
                    /* typeref_id= */  #typeref_id,
                    /* krate= */  #krate,
                    /* marker_path= */ #marker_path,
                    /* implementation= */  [#{args.map(|m| quote!(#m)).unwrap_or(quote!(_))}],
                );
            };
        }
        #[doc(hidden)]
        #{&input.vis} use #temporary_mac_name as #{&input.ident};
    }
}

#[doc(hidden)]
#[proc_macro_error]
#[proc_macro]
pub fn _sumtrait_internal(input: TokenStream1) -> TokenStream1 {
    sumtrait_internal::sumtrait_internal(input.into()).into()
}

#[proc_macro_error]
#[proc_macro_attribute]
pub fn sumtrait(attr: TokenStream1, input: TokenStream1) -> TokenStream1 {
    #[derive(FromMeta, Debug)]
    struct SumtraitArgs {
        implement: Option<Path>,
        krate: Option<Path>,
        marker: Path,
    }
    let args = SumtraitArgs::from_list(&NestedMeta::parse_meta_list(attr.into()).unwrap()).unwrap();

    let krate = args.krate.unwrap_or(parse_quote!(::sumtype));
    sumtrait_impl(
        args.implement,
        &args.marker,
        &krate,
        parse(input).unwrap_or_else(|_| abort!(Span::call_site(), "Requires trait definition")),
    )
    .into()
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
    inner(&parse_macro_input!(attr as Arguments), input.into()).into()
}
