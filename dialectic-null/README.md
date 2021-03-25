This crate contains the "null" backend for Dialectic. If you are a user, you will likely never have
a use for this, as what it does is completely eliminate any transport backend *and* it can only send
and receive the unit type `()`. The null backend is used within Dialectic for testing and
benchmarking purposes, for example to determine how much overhead Dialectic's constructs have over
raw operations.