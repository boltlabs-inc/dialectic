extern crate proc_macro;

use {proc_macro::TokenStream, syn::parse_macro_input};

#[proc_macro]
pub fn session(input: TokenStream) -> TokenStream {
    let invocation = parse_macro_input!(input as dialectic_compiler::Invocation);
    syn::parse_str::<proc_macro2::TokenStream>(&invocation.ast.to_session().to_string())
        .expect("bad output from dialectic-compiler!")
        .into()
}
