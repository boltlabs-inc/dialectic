use dialectic_compiler::Invocation;

mod common;

#[test]
fn hello_invocation() {
    let to_parse = "
                send String;
                recv String;
            ";

    let ast = syn::parse_str::<Invocation>(to_parse).unwrap();
    let s = ast.to_session().unwrap().to_string();
    assert_eq!(s, "Send<String, Recv<String, Done>>");
}

#[test]
fn hello_invocation_double_block() {
    let to_parse = "{
                send String;
                recv String;
            }";

    let ast = syn::parse_str::<Invocation>(to_parse).unwrap();
    let s = ast.to_session().unwrap().to_string();
    assert_eq!(s, "Send<String, Recv<String, Done>>");
}

#[test]
fn basic_split() {
    let to_parse = "split {
                -> send String,
                <- recv String,
            }";

    let ast = syn::parse_str::<Invocation>(to_parse).unwrap();
    let s = ast.to_session().unwrap().to_string();
    assert_eq!(s, "Split<Send<String, Done>, Recv<String, Done>, Done>");
}

#[test]
fn continued_split() {
    let to_parse = "{
            split {
                -> send String,
                <- recv String,
            };
            send bool;
        }";

    let ast = syn::parse_str::<Invocation>(to_parse).unwrap();
    let s = ast.to_session().unwrap().to_string();
    assert_eq!(
        s,
        "Split<Send<String, Done>, Recv<String, Done>, Send<bool, Done>>"
    );
}

#[test]
fn simple_break_outside_of_loop() {
    expect_errors! {
        {
            break;
        } => [
            CompileError::BreakOutsideLoop,
        ]
    };
}

#[test]
fn simple_continue_outside_of_loop() {
    expect_errors! {
        {
            continue;
        } => [
            CompileError::ContinueOutsideLoop,
        ]
    };
}

#[test]
fn shadowed_label() {
    expect_errors! {
        {
            'foo: loop {
                'foo: loop {}
            }
        } => [
            CompileError::UnproductiveLoop,
            CompileError::ShadowedLabel("foo".to_owned()),
        ]
    };
}

#[test]
fn undeclared_label() {
    expect_errors! {
        {
            continue 'foo;
        } => [
            CompileError::ContinueOutsideLoop,
        ]
    };
}

#[test]
fn infinite_loop() {
    expect_errors! {
        {
            loop {
                send ();
            };
            send ()
        } => [
            CompileError::UnreachableStatement,
            CompileError::FollowingCodeUnreachable,
        ]
    };
}

#[test]
fn simple_unproductive_loop() {
    expect_errors! {
        {
            loop {}
        } => [
            CompileError::UnproductiveLoop,
        ]
    };
}

#[test]
fn nested_unproductive_loop() {
    expect_errors! {
        {
            'outer: loop {
                loop {
                    continue 'outer;
                }
            }
        } => [
            CompileError::UnproductiveLoop,
            CompileError::UnproductiveContinue,
        ]
    };
}

#[test]
fn break_unproductive_loop() {
    expect_errors! {
        {
            loop {
                loop {
                    break;
                }
            }
        } => [
            CompileError::UnproductiveLoop,
        ]
    };
}

#[test]
fn very_unproductive_loop() {
    expect_errors! {
        {
            loop {
                loop { break; };
                'outer: loop {
                    loop {
                        break 'outer;
                    };
                    send ();
                };
            }
        } => [
            CompileError::UnreachableStatement,
            CompileError::FollowingCodeUnreachable,
            CompileError::UnproductiveLoop,
        ]
    };
}

#[test]
fn loop_de_loop() {
    expect_errors! {
        {
            loop {} loop {}
        }
        => [
            CompileError::UnproductiveLoop,
            CompileError::UnreachableStatement,
            CompileError::FollowingCodeUnreachable,
        ]
    }
}
