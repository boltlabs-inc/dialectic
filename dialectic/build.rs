use std::fmt::{self, Display, Formatter};
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::{env, error::Error};

fn main() -> Result<(), Box<dyn Error>> {
    // We auto-generate unit tests for session types enumerated within certain size bounds.
    // This makes it much less likely that an error in trait definition will be un-caught.

    // Open a file to write to it in the output directory for the build
    let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("valid_sessions.rs");
    let mut f = File::create(&dest_path).unwrap();

    // Define the sessions we wish to test
    let deep = Session::enumerate(6, 1, 1);
    let medium = Session::enumerate(3, 2, 3);
    let wide = Session::enumerate(2, 4, 127);
    let sessions = deep.chain(medium.chain(wide));

    // File header
    writeln!(f, "#[allow(unused_imports)] use crate::prelude::*;")?;
    writeln!(f, "use static_assertions::assert_impl_all;")?;
    writeln!(f)?;

    // Write out the test
    writeln!(f, "#[test]")?;
    writeln!(f, "fn all_sessions_valid() {{")?;
    for s in sessions {
        writeln!(f, "    assert_impl_all!({}: Session);", s)?;
    }
    writeln!(f, "}}")?;
    Ok(())
}

#[derive(Clone, Debug)]
pub enum Session {
    Done,
    Recv(Box<Session>),
    Send(Box<Session>),
    Choose(Vec<Session>),
    Offer(Vec<Session>),
    Loop(Box<Session>),
    Continue(u8),
    Split(Box<Session>, Box<Session>, Box<Session>),
    Call(Box<Session>, Box<Session>),
}

impl Session {
    pub fn enumerate(max_depth: u8, min_width: u8, max_width: u8) -> SessionIter {
        let session = if max_depth == 0 {
            None
        } else {
            Some(Session::Done)
        };
        SessionIter {
            max_depth,
            min_width,
            max_width,
            session,
        }
    }
}

impl Display for Session {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use Session::*;
        match self {
            Done => write!(f, "Done")?,
            Recv(s) => write!(f, "Recv<(), {}>", s)?,
            Send(s) => write!(f, "Send<(), {}>", s)?,
            Loop(s) => write!(f, "Loop<{}>", s)?,
            Split(s, p, q) => write!(f, "Split<{}, {}, {}>", s, p, q)?,
            Call(s, p) => write!(f, "Call<{}, {}>", s, p)?,
            Choose(cs) => {
                let count = cs.len();
                write!(f, "Choose<(")?;
                for (i, c) in cs.iter().enumerate() {
                    write!(f, "{}", c)?;
                    if i + 1 < count {
                        write!(f, ", ")?;
                    }
                }
                if count == 1 {
                    write!(f, ",")?;
                }
                write!(f, ")>")?;
            }
            Offer(cs) => {
                let count = cs.len();
                write!(f, "Offer<(")?;
                for (i, c) in cs.iter().enumerate() {
                    write!(f, "{}", c)?;
                    if i + 1 < count {
                        write!(f, ", ")?;
                    }
                }
                if count == 1 {
                    write!(f, ",")?;
                }
                write!(f, ")>")?;
            }
            Continue(n) => {
                write!(f, "Continue")?;
                if *n > 0 {
                    write!(f, "<_{}>", n)?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct SessionIter {
    max_depth: u8,
    min_width: u8,
    max_width: u8,
    session: Option<Session>,
}

impl Iterator for SessionIter {
    type Item = Session;

    fn next(&mut self) -> Option<Session> {
        match self.session {
            None => None,
            Some(ref mut session) => {
                let result = session.clone();
                if !session.step(self.max_depth, self.min_width, self.max_width, 0, 0) {
                    self.session = None;
                }
                Some(result)
            }
        }
    }
}

impl Session {
    /// Step this session to the next one in a sequence of enumerated sessions, or return `false` if
    /// this is the last session in such a sequence. This is called within the `Iterator`
    /// implementation for `SessionIter`.
    fn step(
        &mut self,
        max_depth: u8,  // how many type constructors deep can we go?
        min_width: u8,  // what's the min width of tuples in Offer/Choose? (if > 2, no Split/Call)
        max_width: u8,  // what's the max width of tuples in Offer/Choose? (if < 2, no Split/Call)
        loops: u8,      // how many loops are we under (controls Continue)?
        productive: u8, // how many loops up is the nearest non-Loop type?
    ) -> bool {
        use Session::*;

        // Abort if the max depth is zero, because we must produce something of length >= 1
        if max_depth == 0 {
            return false;
        }

        match self {
            Done => {
                if max_depth <= 1 {
                    if loops > 0 && productive < loops {
                        *self = Continue(productive);
                    } else {
                        return false;
                    }
                } else {
                    *self = Recv(Box::new(Done));
                }
            }
            Recv(s) => {
                if !s.step(max_depth - 1, min_width, max_width, loops, 0) {
                    *self = Send(Box::new(Done));
                }
            }
            Send(s) => {
                if !s.step(max_depth - 1, min_width, max_width, loops, 0) {
                    *self = Loop(Box::new(Done));
                }
            }
            Loop(s) => {
                if !s.step(
                    max_depth - 1,
                    min_width,
                    max_width,
                    loops + 1,
                    productive + 1,
                ) {
                    if min_width <= 3 && 3 <= max_width {
                        *self = Split(Box::new(Done), Box::new(Done), Box::new(Done));
                    } else if min_width <= 2 && 2 <= min_width {
                        *self = Call(Box::new(Done), Box::new(Done));
                    } else {
                        let mut initial = Vec::with_capacity(max_width.into());
                        for _ in 0..min_width {
                            initial.push(Done);
                        }
                        *self = Choose(initial);
                    }
                }
            }
            Split(s, p, q) => {
                if !s.step(max_depth - 1, min_width, max_width, loops, 0)
                    && !p.step(max_depth - 1, min_width, max_width, loops, 0)
                    && !q.step(max_depth - 1, min_width, max_width, loops, 0)
                {
                    *self = Call(Box::new(Done), Box::new(Done));
                }
            }
            Call(s, p) => {
                if !s.step(max_depth - 1, min_width, max_width, loops, 0)
                    && !p.step(max_depth - 1, min_width, max_width, loops, 0)
                {
                    let mut initial = Vec::with_capacity(max_width.into());
                    for _ in 0..min_width {
                        initial.push(Done);
                    }
                    *self = Choose(initial);
                }
            }
            Choose(cs) => {
                for c in cs.iter_mut() {
                    if c.step(max_depth - 1, min_width, max_width, loops, 0) {
                        return true;
                    } else {
                        *c = Done;
                    }
                }
                if cs.len() < max_width.into() {
                    cs.push(Done);
                } else {
                    let mut initial = Vec::with_capacity(max_width.into());
                    for _ in 0..min_width {
                        initial.push(Done);
                    }
                    *self = Offer(initial);
                }
            }
            Offer(cs) => {
                for c in cs.iter_mut() {
                    if c.step(max_depth - 1, min_width, max_width, loops, 0) {
                        return true;
                    } else {
                        *c = Done;
                    }
                }
                if cs.len() < max_width.into() {
                    cs.push(Done);
                } else if loops > 0 && productive < loops {
                    *self = Continue(productive);
                } else {
                    return false;
                }
            }
            Continue(n) => {
                if *n + 1 < loops {
                    *n += 1;
                } else {
                    return false;
                }
            }
        };
        true
    }
}
