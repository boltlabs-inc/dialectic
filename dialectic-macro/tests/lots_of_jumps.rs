use dialectic::prelude::*;

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
