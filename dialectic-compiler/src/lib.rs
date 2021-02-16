use std::fmt;

#[derive(Clone, Debug)]
pub enum Session {
    Done,
    Recv(Box<Session>),
    Send(Box<Session>),
    Choose(Vec<Session>),
    Offer(Vec<Session>),
    Loop(Box<Session>),
    Continue(u8),
    Split(Box<Session>, Box<Session>),
    Call(Box<Session>, Box<Session>),
}

impl fmt::Display for Session {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Session::*;
        match self {
            Done => write!(f, "Done")?,
            Recv(s) => write!(f, "Recv<(), {}>", s)?,
            Send(s) => write!(f, "Send<(), {}>", s)?,
            Loop(s) => write!(f, "Loop<{}>", s)?,
            Split(s, p) => write!(f, "Split<{}, {}>", s, p)?,
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
