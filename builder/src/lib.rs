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
        quote! {
            #name: std::option::Option<#ty>
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
        let ty = &field.ty;
        quote! {
            #vis fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
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
        }
    };

    //eprintln!("{:#?}", expanded);

    TokenStream::from(expanded)
}
