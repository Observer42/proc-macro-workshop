extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Ident};

#[proc_macro_derive(Builder)]
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
        unimplemented!()
    };
    //eprintln!("{:#?}", fields);

    let optionized = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        if is_option_ty(ty) {
            quote! {
                #name: #ty
            }
        } else {
            quote! {
                #name: std::option::Option<#ty>
            }
        }
    });

    let initial_values = fields.iter().map(|field| {
        let name = &field.ident;
        quote! {
            #name: None
        }
    });

    let builder_methods = fields.iter().map(|field| {
        let name = &field.ident;
        let mut ty = &field.ty;
        if is_option_ty(ty) {
            ty = unwrap_option_ty(ty);
        }
        quote! {
            #vis fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    });

    let build_fields = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        if is_option_ty(ty) {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
            }
        }
    });

    let expanded = quote! {
        #vis struct #builder_name {
            #(#optionized,)*
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

            #vis fn build(&mut self) -> Result<#type_name, Box<dyn std::error::Error>> {
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

fn unwrap_option_ty(ty: &syn::Type) -> &syn::Type {
    assert!(is_option_ty(ty));
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
