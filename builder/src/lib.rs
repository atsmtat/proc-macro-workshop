use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};
use syn::{
    parse::ParseStream, parse_macro_input, spanned::Spanned, Attribute, Data, DeriveInput, Error,
    Field, Fields, GenericArgument, Ident, LitStr, PathArguments, Result, Token, Type,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn my_builder(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let builder = Builder::new(input);

    let builder_struct = builder.define_struct();
    let builder_accessor = builder.define_builder_constructor();
    let builder_impl = builder.impl_struct();

    let tokens = quote! {
        #builder_struct
        #builder_accessor
        #builder_impl
    };
    proc_macro::TokenStream::from(tokens)
}

struct Builder {
    /// input to Builder derive macro
    input: DeriveInput,

    /// name of the builder struct
    name: Ident,
}

impl Builder {
    fn new(input: DeriveInput) -> Self {
        let name = Ident::new(&format!("{}Builder", &input.ident), Span::call_site());
        Builder { input, name }
    }

    /// Check if `ty` is a syntax tree representing a generic type with a
    /// single argument -- `Target< T >`. If so, return the argument
    /// type T. Otherwise, return None.
    fn is_single_arg_generic_type<'t>(ty: &'t Type, target: &'static str) -> Option<&'t Type> {
        if let Type::Path(type_path) = ty {
            if type_path.path.segments.len() == 1 {
                if let Some(seg) = type_path.path.segments.first() {
                    if seg.ident == target {
                        if let PathArguments::AngleBracketed(gen_args) = &seg.arguments {
                            if gen_args.args.len() == 1 {
                                if let Some(GenericArgument::Type(tp)) = gen_args.args.first() {
                                    return Some(tp);
                                }
                            }
                        }
                    }
                }
            }
        }
        None
    }

    /// Define the builder struct `XBuilder` corresponding to the input struct
    /// `X`. A field of type `T` in the input struct gets mapped to a field with
    /// the same name in the builder struct. The type of the builder field is
    /// generated as `Option< T >` when `T` is not an Option type. Otherwise,
    /// it's kept the same as input field.
    /// For example, the following input struct ...
    ///
    /// #[derive(Builder)]
    /// pub struct Command {
    ///    executable: String,
    ///    args: Vec<String>,
    ///    current_dir: Option<String>,
    /// }
    ///
    /// ... is mapped to the following builder struct
    ///
    /// pub struct CommandBuilder {
    ///    executable: Option<String>,
    ///    args: Option<Vec<String>>,
    ///    current_dir: Option<String>,
    /// }
    fn define_struct(&self) -> TokenStream {
        let vis = &self.input.vis;
        let fields = match &self.input.data {
            Data::Struct(ref data) => match &data.fields {
                Fields::Named(ref fields) => {
                    let new_fields = fields.named.iter().map(|f| {
                        let fname = &f.ident;
                        let fty = &f.ty;
                        let build_fty = if Self::is_single_arg_generic_type(fty, "Option").is_some()
                        {
                            quote! { #fty }
                        } else {
                            quote! { std::option::Option< #fty > }
                        };
                        quote_spanned! { f.span()=> #fname : #build_fty }
                    });

                    quote! { #(#new_fields),* }
                }
                Fields::Unnamed(_) | Fields::Unit => {
                    unimplemented!()
                }
            },

            Data::Enum(_) => unimplemented!(),
            Data::Union(_) => unimplemented!(),
        };
        let builder_name = &self.name;
        quote! {
            #vis struct #builder_name {
                #fields
            }
        }
    }

    /// Define a `builder` function associated with the input struct. It returns
    /// a new default-initialized builder object. For example:
    ///
    /// impl Command {
    ///    pub fn builder() -> CommandBuilder {
    ///        CommandBuilder {
    ///            executable: None,
    ///            args: None,
    ///            current_dir: None,
    ///        }
    ///    }
    /// }
    fn define_builder_constructor(&self) -> TokenStream {
        let input_name = &self.input.ident;
        let fields_init = match &self.input.data {
            Data::Struct(ref data) => {
                let init = data.fields.iter().map(|f| {
                    let name = &f.ident;
                    quote_spanned! { f.span()=> #name : std::option::Option::None }
                });
                quote! {
                    #(#init),*
                }
            }
            Data::Enum(_) | Data::Union(_) => unimplemented!(),
        };
        let builder_name = &self.name;
        quote! {
            impl #input_name {
                pub fn builder() -> #builder_name {
                    #builder_name {
                        #fields_init
                    }
                }
            }
        }
    }

    /// Generate impl of the builder struct.
    fn impl_struct(&self) -> TokenStream {
        let field_setters = self.impl_field_setters();
        let build_method = self.impl_build();
        let builder_name = &self.name;
        quote! {
            impl #builder_name {
                #field_setters
                #build_method
            }
        }
    }

    /// Check if the field has an inert attribute with "builder" path like below:
    /// #[builder(...)]
    fn has_builder_attr(field: &Field) -> Option<&Attribute> {
        for attr in &field.attrs {
            if attr.path().is_ident("builder") {
                return Some(attr);
            }
        }
        None
    }

    /// Parse argument to `builder` attribute, expecting the following syntax:
    /// #[builder(each="XYZ")]
    ///           ^^^^^^^^^^  <- parse this input
    /// Return the string literal "XYZ" if input is valid.
    fn parse_builder_attr_arg(input: ParseStream) -> Result<LitStr> {
        let each_token: Ident = input.parse()?;
        if each_token != "each" {
            return Err(Error::new(each_token.span(), "expected `each`"));
        }
        input.parse::<Token![=]>()?;
        let s: LitStr = input.parse()?;
        Ok(s)
    }

    /// Generate methods on the builder for setting a value of each of the
    /// builder struct fields.
    ///
    /// impl CommandBuilder {
    ///    fn executable(&mut self, executable: String) -> &mut Self { ... }
    /// }
    fn impl_field_setters(&self) -> TokenStream {
        match &self.input.data {
            Data::Struct(ref data) => match &data.fields {
                Fields::Named(ref fields) => {
                    let methods = fields.named.iter().map(|f| {
                        if let Some(attr) = Self::has_builder_attr(f) {
                            match attr.parse_args_with(Self::parse_builder_attr_arg) {
                                Ok(each_name) => Self::impl_each_builder(&each_name, f),
                                Err(e) => e.into_compile_error(),
                            }
                        } else {
                            Self::impl_field_builder(f)
                        }
                    });
                    quote! {
                        #(#methods)*
                    }
                }
                Fields::Unnamed(_) | Fields::Unit => unimplemented!(),
            },
            Data::Enum(_) | Data::Union(_) => unimplemented!(),
        }
    }

    /// Generate setter method(s) on the builder's field which has
    /// `[builder(each = "...")]` attribute defined on it. The string argument
    /// to `each` is used as the setter name. If the name is different from the
    /// field namen, then two different setters are generated for the field --
    /// one-at-a-time and all-at-once. If the name conflicts with the field
    /// name, then all-at-once method is skipped.
    ///
    /// The following input field `args` ...
    ///
    /// #[derive(Builder)]
    /// pub struct Command {
    ///    #[builder(each = "arg")]
    ///    args: Vec<String>,
    /// }
    ///
    /// ... generates the following setters on the corresponding builder struct:
    ///
    /// impl CommandBuilder {
    ///    fn arg(&mut self, new_arg: String) -> &mut Self {
    ///       if let Some(v) = self.args.as_mut() {
    ///          v.push(new_arg);
    ///       } else {
    ///          self.args = Some(vec![new_arg]);
    ///       }
    ///       self
    ///    }
    ///
    ///    fn args(&mut self, args: Vec<String>) -> &mut Self {
    ///       self.args = Some(args);
    ///       self
    ///    }
    /// }
    fn impl_each_builder(name: &LitStr, field: &Field) -> TokenStream {
        let elem_ty = Self::is_single_arg_generic_type(&field.ty, "Vec").unwrap();
        let name: Ident = name.parse().unwrap();
        let arg = Ident::new(&format!("new_{}", &name), Span::call_site());
        let field_name = &field.ident;

        let each = quote_spanned! {
            field.span() =>
            fn #name(&mut self, #arg: #elem_ty) -> &mut Self {
                if let std::option::Option::Some(v) = self.#field_name.as_mut() {
                    v.push(#arg);
                } else {
                    self.#field_name = std::option::Option::Some(vec![#arg]);
                }
                self
            }
        };
        if Some(name) == field.ident {
            each
        } else {
            let all = Self::impl_field_builder(field);
            quote! {
                #each
                #all
            }
        }
    }

    /// Generate a setter method on the builder field which sets the field's
    /// value.
    ///
    /// impl CommandBuilder {
    ///    fn executable(&mut self, executable: String) -> &mut Self {
    ///       self.executable = Some(executable);
    ///       self
    ///    }
    /// }
    fn impl_field_builder(field: &Field) -> TokenStream {
        let name = &field.ident;
        let fty = &field.ty;
        let build_fty = if let Some(t) = Self::is_single_arg_generic_type(fty, "Option") {
            t
        } else {
            fty
        };
        quote_spanned! {
            field.span()=>
            fn #name(&mut self, #name: #build_fty) -> &mut Self {
                self.#name = std::option::Option::Some(#name);
                self
            }
        }
    }

    /// Generate a `build` method to build the instance of the original struct
    /// from the builder object.
    ///
    /// impl CommandBuilder {
    ///    pub fn build(&mut self) -> Result<Command, Box<dyn Error>> {
    ///        ...
    /// }
    ///
    /// It returns an error if any non-optional (in the input) field is missing
    /// a value.
    fn impl_build(&self) -> TokenStream {
        let field_inits = match &self.input.data {
            Data::Struct(ref data) => match &data.fields {
                Fields::Named(ref fields) => {
                    let inits = fields.named.iter().map(|f| {
                        let name = &f.ident;
                        let name_str = format!("{}", name.as_ref().unwrap());
                        if Self::is_single_arg_generic_type(&f.ty, "Option").is_some() {
                            quote_spanned! {
                                f.span() =>
                                    #name : self.#name.take()
                            }
                        } else {
                            let err = quote! {
                                format!("field {} is missing", #name_str)
                            };
                            quote_spanned! {
                                f.span()=>
                                #name : self.#name.take().ok_or(#err)?
                            }
                        }
                    });
                    quote! {
                        #(#inits),*
                    }
                }
                Fields::Unnamed(_) | Fields::Unit => unimplemented!(),
            },
            Data::Enum(_) | Data::Union(_) => unimplemented!(),
        };

        let input_name = &self.input.ident;
        quote! {
            pub fn build(&mut self) ->
                std::result::Result<#input_name, std::boxed::Box<dyn std::error::Error>> {
                Ok(#input_name {
                    #field_inits
                })
            }
        }
    }
}
