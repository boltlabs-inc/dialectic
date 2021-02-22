use dialectic_compiler::{CompileError, Invocation};

#[test]
fn hello_invocation() {
    let to_parse = "
                send String;
                recv String;
            ";

    let ast = syn::parse_str::<Invocation>(to_parse).unwrap().syntax;
    let s = format!("{}", ast.to_session().unwrap());
    assert_eq!(s, "Send<String, Recv<String, Done>>");
}

#[test]
fn hello_invocation_double_block() {
    let to_parse = "{
                send String;
                recv String;
            }";

    let ast = syn::parse_str::<Invocation>(to_parse).unwrap().syntax;
    let s = format!("{}", ast.to_session().unwrap());
    assert_eq!(s, "Send<String, Recv<String, Done>>");
}

#[test]
fn basic_split() {
    let to_parse = "split {
                -> send String,
                <- recv String,
            }";

    let ast = syn::parse_str::<Invocation>(to_parse).unwrap().syntax;
    let s = format!("{}", ast.to_session().unwrap());
    assert_eq!(s, "Split<Send<String, Done>, Recv<String, Done>, Done>");
}

#[test]
fn simple_break_outside_of_loop() {
    let to_parse = "break";
    let error = syn::parse_str::<Invocation>(to_parse)
        .unwrap()
        .syntax
        .to_session()
        .unwrap_err();
    assert_eq!(
        error.to_string(),
        CompileError::BreakOutsideLoop.to_string()
    );
}

#[test]
fn simple_continue_outside_of_loop() {
    let to_parse = "continue";
    let error = syn::parse_str::<Invocation>(to_parse)
        .unwrap()
        .syntax
        .to_session()
        .unwrap_err();
    assert_eq!(
        error.to_string(),
        CompileError::ContinueOutsideLoop.to_string()
    );
}

#[test]
fn shadowed_label() {
    let to_parse = "'foo: loop { 'foo: loop {} }";
    let error = syn::parse_str::<Invocation>(to_parse)
        .unwrap()
        .syntax
        .to_session()
        .unwrap_err();
    assert_eq!(
        error.to_string(),
        CompileError::ShadowedLabel("foo".to_owned()).to_string()
    );
}

#[test]
fn undeclared_label() {
    let to_parse = "{ continue 'foo; }";
    let error = syn::parse_str::<Invocation>(to_parse)
        .unwrap()
        .syntax
        .to_session()
        .unwrap_err();
    assert_eq!(
        error.to_string(),
        CompileError::UndeclaredLabel("foo".to_owned()).to_string()
    );
}
