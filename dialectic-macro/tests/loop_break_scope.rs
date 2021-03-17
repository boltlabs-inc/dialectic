use dialectic::prelude::*;
use dialectic::types::*;
use static_assertions::assert_type_eq_all;

#[allow(dead_code)]
type Bug = Session! {
    'outer: loop {
        loop {
            break;
        };
        send ();
    }
};

assert_type_eq_all!(Bug, Loop<Loop<Send<(), Continue<1>>>>,);
