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

    let builder_fields = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        if is_option_ty(ty) || builder_attr(&field).is_some() {
            quote! { #name: #ty }
        } else {
            quote! { #name: std::option::Option<#ty> }
        }
    });

    let initial_values = fields.iter().map(|field| {
        let name = &field.ident;
        if builder_attr(&field).is_some() {
            quote! { #name: std::vec::Vec::new() }
        } else {
            quote! { #name: std::option::Option::None }
        }
    });

    let builder_methods = fields.iter().map(|field| {
        let name = &field.ident;
        let mut ty = &field.ty;
        if is_option_ty(ty) {
            ty = unwrap_option_ty(ty);
        }
        if let Some(attr) = builder_attr(&field) {
            match &parse_attr(attr) {
                Ok(each_name) => {
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
                Err(_) => panic!("invalid attr"),
            }
        } else {
            quote! {
                #vis fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            }
        }
    });

    let build_fields = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        if is_option_or_vec_ty(ty) {
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

fn is_option_or_vec_ty(ty: &syn::Type) -> bool {
    if let syn::Type::Path(ref tp) = ty {
        if tp.path.segments.len() == 1 {
            let ident = &tp.path.segments[0].ident;
            return ident == "Option" || ident == "Vec";
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

fn parse_attr(attr: &syn::Attribute) -> Result<syn::Ident, Box<dyn std::error::Error>> {
    let meta = attr.parse_meta()?;
    let lit = match meta {
        syn::Meta::List(meta_list) => {
            if meta_list.nested.len() != 1 {
                Err("wrong attr format!".into())
            } else {
                if let syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) = &meta_list.nested[0] {
                    Ok(nv.lit.clone())
                } else {
                    Err("wrong attr format!".into())
                }
            }
        }
        _ => Err("wrong attr format!".to_owned()),
    }?;
    match lit {
        syn::Lit::Str(s) => Ok(syn::Ident::new(&s.value(), s.span())),
        _ => Err("wrong attr format!".into()),
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
