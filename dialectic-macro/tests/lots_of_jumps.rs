use dialectic::prelude::*;
use dialectic::types::*;
use static_assertions::assert_type_eq_all;

#[allow(dead_code)]
type LabelExample = Session! {
    'outer: loop {
        send i64;
        loop {
            recv bool;
            offer {
                _0 => break 'outer,
                _1 => continue 'outer,
                _2 => break,
                _3 => continue,
                _4 => send String,
            };
            send bool;
        };
        recv i64;
    }
};

assert_type_eq_all!(
    LabelExample,
    Loop<
        Send<
            i64,
            Loop<
                Recv<
                    bool,
                    Offer<(
                        Done,
                        Continue<1>,
                        Recv<i64, Continue<1>>,
                        Continue<0>,
                        Send<String, Send<bool, Continue<0>>>
                    )>,
                >,
            >,
        >,
    >,
);
