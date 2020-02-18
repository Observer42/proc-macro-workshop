#![recursion_limit = "128"]

extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Ident};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    //eprintln!("{:#?}", ast);

    let type_name = &ast.ident;
    let builder_name = Ident::new(&format!("{}Builder", type_name), type_name.span());
    let vis = &ast.vis;

    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        panic!("currently only support struct with named fields");
    };
    //eprintln!("{:#?}", fields);

    let attributes = fields.iter().map(|field| builder_attr(&field));

    let attr_contents = attributes
        .clone()
        .map(|attr| attr.and_then(|a| parse_attr(a)))
        .collect::<Vec<_>>();

    let builder_fields = fields
        .iter()
        .zip(attr_contents.iter())
        .map(|(field, attr_inner)| {
            let name = &field.ident;
            let ty = &field.ty;
            if is_option_ty(ty) || attr_inner.is_some() {
                quote! { #name: #ty }
            } else {
                quote! { #name: std::option::Option<#ty> }
            }
        });

    let initial_values = fields
        .iter()
        .zip(attr_contents.iter())
        .map(|(field, attr_inner)| {
            let name = &field.ident;
            if attr_inner.is_some() {
                quote! { #name: std::vec::Vec::new() }
            } else {
                quote! { #name: std::option::Option::None }
            }
        });

    let builder_methods = fields.iter().zip(attr_contents.iter()).zip(attributes).map(
        |((field, attr_inner), attr)| {
            let name = &field.ident;
            let mut ty = &field.ty;
            if is_option_ty(ty) {
                ty = unwrap_option_ty(ty);
            }
            if attr.is_some() {
                match attr_inner {
                    Some(each_name) => {
                        let each_ty = unwrap_ty(ty);
                        if name.as_ref().unwrap() == each_name {
                            quote! {
                                #vis fn #each_name(&mut self, #each_name: #each_ty) -> &mut Self {
                                    self.#name.push(#each_name);
                                    self
                                }
                            }
                        } else {
                            quote! {
                                #vis fn #name(&mut self, #name: #ty) -> &mut Self {
                                    self.#name = #name;
                                    self
                                }

                                #vis fn #each_name(&mut self, #each_name: #each_ty) -> &mut Self {
                                    self.#name.push(#each_name);
                                    self
                                }
                            }
                        }
                    }
                    None => {
                        let meta = attr.unwrap().parse_meta().unwrap();
                        let err = match &meta {
                            syn::Meta::List(meta_list) => syn::Error::new_spanned(
                                meta_list,
                                "expected `builder(each = \"...\")`",
                            )
                            .to_compile_error(),
                            _ => {
                                syn::Error::new_spanned(meta, "expected `builder(each = \"...\")`")
                                    .to_compile_error()
                            }
                        };
                        quote! { #err }
                    }
                }
            } else {
                quote! {
                    #vis fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = std::option::Option::Some(#name);
                        self
                    }
                }
            }
        },
    );

    let build_fields = fields.iter().zip(attr_contents.iter()).map(|(field, attr_inner)| {
        let name = &field.ident;
        let ty = &field.ty;
        if is_option_ty(ty) || attr_inner.is_some() {
            quote! { #name: self.#name.clone() }
        } else {
            quote! { #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))? }
        }
    });

    let expanded = quote! {
        #vis struct #builder_name {
            #(#builder_fields,)*
        }

        impl #type_name {
            #vis fn builder() -> #builder_name {
                #builder_name {
                    #(#initial_values,)*
                }
            }
        }

        impl #builder_name {
            #(#builder_methods)*

            #vis fn build(&mut self) -> std::result::Result<#type_name, std::boxed::Box<dyn std::error::Error>> {
                Ok(#type_name {
                    #(#build_fields,)*
                })
            }
        }
    };

    TokenStream::from(expanded)
}

fn is_option_ty(ty: &syn::Type) -> bool {
    if let syn::Type::Path(ref tp) = ty {
        if tp.path.segments.len() == 1 && tp.path.segments[0].ident == "Option" {
            return true;
        }
    }
    false
}

fn builder_attr(ty: &syn::Field) -> Option<&syn::Attribute> {
    for attr in &ty.attrs {
        if attr.path.segments.len() == 1 && attr.path.segments[0].ident == "builder" {
            return Some(&attr);
        }
    }
    None
}

fn parse_attr(attr: &syn::Attribute) -> Option<syn::Ident> {
    let meta = attr.parse_meta().unwrap();
    let lit = match &meta {
        syn::Meta::List(meta_list) => {
            if meta_list.nested.len() != 1 {
                None
            } else {
                if let syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) = &meta_list.nested[0] {
                    if nv.path.segments.len() == 1 && nv.path.segments[0].ident == "each" {
                        Some(nv.lit.clone())
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        }
        _ => None,
    }?;
    match lit {
        syn::Lit::Str(s) => Some(syn::Ident::new(&s.value(), s.span())),
        _ => None,
    }
}

fn unwrap_option_ty(ty: &syn::Type) -> &syn::Type {
    assert!(is_option_ty(ty));
    unwrap_ty(ty)
}

fn unwrap_ty(ty: &syn::Type) -> &syn::Type {
    if let syn::Type::Path(ref tp) = ty {
        if let syn::PathArguments::AngleBracketed(ref bracketed_args) =
            tp.path.segments[0].arguments
        {
            assert_eq!(bracketed_args.args.len(), 1);
            let path = &bracketed_args.args[0];
            if let syn::GenericArgument::Type(unwrapped) = path {
                return unwrapped;
            }
        }
    }
    unreachable!()
}
