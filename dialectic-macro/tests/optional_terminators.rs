use dialectic::prelude::*;
use dialectic::types::*;
use static_assertions::assert_type_eq_all;

struct Foo;

#[allow(dead_code)]
type BigChoose = Session! {
    choose {
        0 => send (),
        1 => { send () }
        2 => call { Foo }
        3 => call Foo,
        4 => split {
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
