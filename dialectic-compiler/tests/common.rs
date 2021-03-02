#[macro_export]
macro_rules! expect_errors {
    ({$($syntax:tt)*} => [$($err:expr,)*]) => {{
        use {
            dialectic_compiler::{CompileError, Invocation},
            std::collections::HashSet,
        };

        let syntax = stringify!($($syntax)*);
        let err_set = syn::parse_str::<Invocation>(syntax)
            .unwrap()
            .syntax
            .to_session()
            .unwrap_err()
            .into_iter()
            .map(|err| err.to_string())
            .collect::<HashSet<_>>();
        let expected_errs: &[CompileError] = &[$($err),*];
        let expected_set = expected_errs.iter().map(|err| err.to_string()).collect::<HashSet<_>>();

        assert_eq!(err_set, expected_set, "unexpected set of errors");
    }};
}
