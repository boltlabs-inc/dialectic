extern crate proc_macro;

use {dialectic_compiler::Soup, proc_macro::TokenStream, syn::parse_macro_input};

#[proc_macro]
pub fn session(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as dialectic_compiler::Ast);
    let mut soup = Soup::new();
    let cfg = soup.to_cfg(&ast);
    soup.eliminate_breaks_and_labels(&mut Vec::new(), cfg);
    let session = soup[cfg].to_session(&soup, &mut Vec::new());
    syn::parse_str::<proc_macro2::TokenStream>(&session.to_string())
        .expect("bad output from dialectic-compiler!")
        .into()
}
