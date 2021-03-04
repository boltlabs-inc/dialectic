use {
    dialectic_compiler::{Spanned, Syntax},
    quickcheck::{Gen, QuickCheck, TestResult},
    quote::ToTokens,
};

fn parser_roundtrip_property(syntax: Spanned<Syntax>) -> TestResult {
    let syntax_tokens = syntax.to_token_stream();
    let mut g = Gen::new(0);
    match syn::parse2::<Spanned<Syntax>>(
        syntax.to_token_stream_with(&mut || *g.choose(&[true, false]).unwrap()),
    ) {
        Ok(parsed) => {
            TestResult::from_bool(syntax_tokens.to_string() == parsed.to_token_stream().to_string())
        }
        Err(error) => TestResult::error(format!(
            "failed w/ parse string {}, error: {}",
            syntax_tokens, error
        )),
    }
}

#[test]
fn parser_roundtrip() {
    QuickCheck::new()
        .gen(Gen::new(13))
        .quickcheck(parser_roundtrip_property as fn(_) -> TestResult)
}
