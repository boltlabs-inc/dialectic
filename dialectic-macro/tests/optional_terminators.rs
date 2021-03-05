use dialectic::prelude::*;
use dialectic::types::*;
use static_assertions::assert_type_eq_all;

struct Foo;

#[allow(dead_code)]
type BigChoose = Session! {
    choose {
        _0 => send (),
        _1 => { send () }
        _2 => call { Foo }
        _3 => call Foo,
        _4 => split {
            -> {
                send String;
            }
            <- recv String
        }
    }
};

assert_type_eq_all!(
    BigChoose,
    Choose<(
        Send<(), Done>,
        Send<(), Done>,
        Call<Foo, Done>,
        Call<Foo, Done>,
        Split<Send<String, Done>, Recv<String, Done>, Done>
    )>
);
