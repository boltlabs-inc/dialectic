use dialectic::{prelude::*, types::*};
use static_assertions::assert_type_eq_all;

#[allow(dead_code)]
type Bug = Session! {
    loop {
        choose {
            0 => {},
        }
    }
};

assert_type_eq_all!(Bug, Loop<Choose<(Continue<0>,), Choice<1>>>,);
