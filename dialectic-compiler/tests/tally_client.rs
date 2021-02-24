use {
    dialectic_compiler::{Invocation, Spanned, Syntax},
    quote::ToTokens,
    syn::Type,
};

#[test]
fn tally_client_expr_call_ast() {
    let client_ast: Spanned<Syntax> = Syntax::Loop(
        None,
        Box::new(
            Syntax::Choose(vec![
                Syntax::Break(None).into(),
                Syntax::Block(vec![
                    Syntax::send("Operation").into(),
                    Syntax::call(Syntax::type_("ClientTally")).into(),
                ])
                .into(),
            ])
            .into(),
        ),
    )
    .into();

    let s = format!("{}", client_ast.to_session().unwrap());
    assert_eq!(
        s,
        "Loop<Choose<(Done, Send<Operation, Call<ClientTally, Continue>>)>>"
    );
}

#[test]
fn tally_client_expr_call_parse_string() {
    let to_parse = "loop {
            choose {
                _0 => break,
                _1 => {
                    send Operation;
                    call ClientTally;
                },
            }
        }";

    let ast = syn::parse_str::<Spanned<Syntax>>(to_parse).unwrap();
    let s = format!("{}", ast.to_session().unwrap());
    assert_eq!(
        s,
        "Loop<Choose<(Done, Send<Operation, Call<ClientTally, Continue>>)>>"
    );
}

#[test]
fn tally_client_invocation_call_parse_string() {
    let to_parse = "loop {
                choose {
                    _0 => break,
                    _1 => {
                        send Operation;
                        call ClientTally;
                    },
                }
            }";

    let ast = syn::parse_str::<Spanned<Syntax>>(to_parse).unwrap();
    let s = format!("{}", ast.to_session().unwrap());
    assert_eq!(
        s,
        "Loop<Choose<(Done, Send<Operation, Call<ClientTally, Continue>>)>>"
    );
}

#[test]
fn tally_client_invocation_direct_subst_parse_string() {
    let to_parse = "'client: loop {
                choose {
                    _0 => break,
                    _1 => {
                        send Operation;
                        ClientTally;
                    },
                }
            }";

    let ast = syn::parse_str::<Spanned<Syntax>>(to_parse).unwrap();
    let s = format!("{}", ast.to_session().unwrap());
    assert_eq!(
        s,
        "Loop<Choose<(Done, Send<Operation, <ClientTally as Then<Continue>>::Combined>)>>"
    );
}

#[test]
fn tally_client_direct_subst_nested_loop_break() {
    let to_parse = "'client: loop {
            choose {
                _0 => break,
                _1 => {
                    send Operation;
                    loop {
                        choose {
                            _0 => send i64,
                            _1 => {
                                recv i64;
                                continue 'client;
                            }
                        }
                    }
                },
            }
        }";

    let lhs: Type = syn::parse2(
        syn::parse_str::<Invocation>(to_parse)
            .unwrap()
            .syntax
            .to_session()
            .unwrap()
            .into_token_stream(),
    )
    .unwrap();

    let rhs: Type = syn::parse_str(
            "dialectic::types::Loop<
                dialectic::types::Choose<(
                    dialectic::types::Done,
                    dialectic::types::Send<
                        Operation,
                        dialectic::types::Loop<
                            dialectic::types::Choose<(
                                dialectic::types::Send<i64, dialectic::types::Continue>,
                                dialectic::types::Recv<i64, dialectic::types::Continue<dialectic::unary::S<dialectic::unary::Z>>>,
                            )>
                        >
                    >,
                )>
            >",
        )
        .unwrap();

    assert_eq!(
        lhs.to_token_stream().to_string(),
        rhs.to_token_stream().to_string()
    );
}
