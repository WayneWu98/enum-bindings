extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::Parse;
use syn::{parse_macro_input, Attribute, DeriveInput, Meta, Variant};

#[proc_macro_attribute]
pub fn derive_meta(attr: TokenStream, input: TokenStream) -> TokenStream {
    let meta = parse_macro_input!(attr as Meta);
    let t = meta.require_path_only().unwrap().get_ident().unwrap();
    // let list = meta.require_list().unwrap();
    let ast = parse_macro_input!(input as DeriveInput);
    // println!("list.path, {:?}", list);
    // list.path.segments.iter().for_each(|seg| {
    //     println!("seg.ident, {:?}", seg.ident);
    // });
    // quote! {
    //     #ast

    //     // #t
    // }
    // .into()
    // let enum_type = meta.require_path_only().unwrap();

    // let ast = parse_macro_input!(input as DeriveInput);
    let enum_ident = ast.ident.clone();

    let variants = match ast.data {
        syn::Data::Enum(ref data) => data.variants.iter().map(|variant| {
            let variant_ident: syn::Ident = variant.ident.clone();
            let ident = variant
                .attrs
                .iter()
                .find(|attr| attr.path().is_ident("meta"))
                .and_then(|v| v.meta.require_list().ok())
                .and_then(|list| Some(list.tokens.clone()));
            if let Some(ident) = ident {
                quote! { #enum_ident::#variant_ident => #ident }
            } else {
                quote! { #enum_ident::#variant_ident => Default::default() }
            }
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

        impl emeta::Meta for #enum_ident {
            type MetaType = #t;
            fn meta(&self) -> Self::MetaType {
                match self {
                    #(#variants),*
                }
            }
        }
    };

    gen.into()
}

#[proc_macro_attribute]
pub fn meta(attr: TokenStream, input: TokenStream) -> TokenStream {
    let meta = parse_macro_input!(attr as Meta);
    let t = meta.require_path_only().unwrap().get_ident().unwrap();
    input
    // let t = meta.require_path_only().unwrap().get_ident().unwrap();
}
