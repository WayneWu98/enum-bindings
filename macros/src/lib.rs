extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::{format_ident, quote, ToTokens};
use syn::{parse_macro_input, Expr, Variant};
use syn::{DeriveInput, Meta};

fn default_expr() -> Expr {
    syn::parse_str::<Expr>("Default::default()").unwrap()
}

fn make_expr(s: &str) -> Expr {
    syn::parse_str::<Expr>(s).unwrap()
}

fn unnamed_pattern(
    enum_ident: &Ident,
    variant_ident: &Ident,
    extractor: Vec<String>,
    output: Option<&Expr>,
    asterisks: usize,
) -> proc_macro2::TokenStream {
    let default = default_expr();
    let output = format!(
        "{}{}",
        vec!["*"; asterisks].join(""),
        output.unwrap_or(&default).to_token_stream().to_string()
    );
    let output = syn::parse_str::<Expr>(&output).ok();
    let extractor = extractor
        .iter()
        .map(|v| format_ident!("{}", v))
        .collect::<Vec<_>>();
    quote! { #enum_ident::#variant_ident(#(#extractor),*) => #output }
}
fn named_pattern(
    enum_ident: &Ident,
    variant_ident: &Ident,
    members: &Vec<Ident>,
    output: Option<&Expr>,
    asterisks: usize,
) -> proc_macro2::TokenStream {
    let default = default_expr();
    let output = format!(
        "{}{}",
        vec!["*"; asterisks].join(""),
        output.unwrap_or(&default).to_token_stream().to_string()
    );
    let output = syn::parse_str::<Expr>(&output).ok();
    quote! { #enum_ident::#variant_ident{ #(#members),* } => #output }
}
fn unit_pattern(
    enum_ident: &Ident,
    variant_ident: &Ident,
    output: Option<&Expr>,
    asterisks: usize,
) -> proc_macro2::TokenStream {
    let default = default_expr();
    let output = format!(
        "{}{}",
        vec!["*"; asterisks].join(""),
        output.unwrap_or(&default).to_token_stream().to_string()
    );
    let output = syn::parse_str::<Expr>(&output).ok();
    quote! { #enum_ident::#variant_ident => #output }
}

fn pattern(
    enum_ident: &Ident,
    variant: &Variant,
    extractor: Option<&Expr>,
    output: Option<&Expr>,
    asterisks: usize,
) -> proc_macro2::TokenStream {
    let variant_ident = &variant.ident;
    match &variant.fields {
        syn::Fields::Named(named) => {
            let names = named
                .named
                .iter()
                .map(|v| v.ident.clone())
                .filter(|v| v.is_some())
                .map(|v| v.unwrap())
                .collect::<Vec<_>>();
            named_pattern(enum_ident, variant_ident, &names, output, asterisks)
        }
        syn::Fields::Unnamed(named) => {
            let len = named.unnamed.len();
            let pos = extractor
                .to_token_stream()
                .to_string()
                .parse::<usize>()
                .unwrap_or(0);
            let leading = vec!["_"; pos];
            let trailing = vec!["_"; len - pos - 1];
            let extractor = leading
                .iter()
                .chain(vec!["v"].iter())
                .chain(trailing.iter())
                .map(|v| v.to_string())
                .collect::<Vec<_>>();
            unnamed_pattern(enum_ident, variant_ident, extractor, output, asterisks)
        }
        syn::Fields::Unit => unit_pattern(enum_ident, variant_ident, output, asterisks),
    }
}

#[proc_macro_attribute]
pub fn derive_meta(attr: TokenStream, input: TokenStream) -> TokenStream {
    let meta_type = parse_macro_input!(attr as Meta)
        .require_path_only()
        .unwrap()
        .get_ident()
        .and_then(|v| Some(v.clone()))
        .unwrap();
    let ast = parse_macro_input!(input as DeriveInput);
    let enum_ident = ast.ident.clone();
    let variants = match ast.data {
        syn::Data::Enum(ref data) => data.variants.iter().map(|variant| {
            let meta = variant
                .attrs
                .iter()
                .find(|attr| attr.path().is_ident("meta"))
                .and_then(|v| v.meta.require_list().ok())
                .and_then(|list| Some(list.tokens.to_string().trim().to_string()))
                .and_then(|s| {
                    let leading_asterisks = s.chars().take_while(|c| *c == '*').count();
                    Some((
                        s.chars().skip(leading_asterisks).collect::<String>(),
                        leading_asterisks,
                    ))
                });
            if let Some((meta, asterisks)) = meta {
                let expr = meta.trim().to_string();
                if expr.starts_with("self") {
                    // #[meta(self.xxxxxx)]
                    let expr_splitted = expr.split('.').skip(1).collect::<Vec<&str>>();
                    let first_expr = expr_splitted.first();
                    if let Some(first) = first_expr {
                        let first = first.trim();
                        if let Some(num) = first.parse::<usize>().ok() {
                            // #[meta(self.0)] / #[meta(self.1)] / #[meta(self.2)]
                            let mut output =
                                expr_splitted[1..].iter().map(|v| *v).collect::<Vec<_>>();
                            output.insert(0, "v");
                            let output = syn::parse_str::<Expr>(&output.join(".")).unwrap();
                            return pattern(
                                &enum_ident,
                                variant,
                                Some(&make_expr(num.to_string().as_str())),
                                Some(&output),
                                asterisks,
                            );
                        }
                        // #[meta(self.alphabet)]
                        return pattern(
                            &enum_ident,
                            variant,
                            Some(&make_expr(first)),
                            Some(&make_expr(&expr_splitted.join("."))),
                            asterisks,
                        );
                    }
                    // #[meta(self)]
                    return pattern(
                        &enum_ident,
                        variant,
                        None,
                        Some(&make_expr("self")),
                        asterisks,
                    );
                }
                return pattern(
                    &enum_ident,
                    variant,
                    None,
                    Some(&make_expr(&meta)),
                    asterisks,
                );
            }
            pattern(&enum_ident, variant, None, None, 0)
        }),
        _ => panic!("Only enums are supported"),
    };

    let mut cloned = ast.clone();

    if let syn::Data::Enum(ref mut data) = cloned.data {
        data.variants.iter_mut().for_each(|variant| {
            variant.attrs.retain(|attr| !attr.path().is_ident("meta"));
        });
    }

    let gen = quote! {
        #cloned

        impl enum_meta::Meta for #enum_ident {
            type MetaType = #meta_type;
            fn meta(&self) -> Self::MetaType {
                match self {
                    #(#variants),*
                }
            }
        }
    };

    gen.into()
}
