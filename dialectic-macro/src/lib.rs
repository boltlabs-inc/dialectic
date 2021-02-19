extern crate proc_macro;

use {proc_macro::TokenStream, quote::ToTokens, syn::parse_macro_input};

#[proc_macro]
#[allow(non_snake_case)]
pub fn Session(input: TokenStream) -> TokenStream {
    parse_macro_input!(input as dialectic_compiler::Invocation)
        .syntax
        .to_session()
        .to_token_stream()
        .into()
}
