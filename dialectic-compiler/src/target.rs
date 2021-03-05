//! The target language of the `Session!` macro, produced by the compiler.

use {
    lazy_static::lazy_static,
    proc_macro2::TokenStream,
    quote::{quote_spanned, ToTokens},
    std::{fmt, rc::Rc},
    syn::{Ident, Type},
};

use crate::Spanned;

/// The target language of the macro: the type level language of session types in Dialectic.
///
/// This is a one-to-one mapping to the literal syntax you would write without using the `Session!`
/// macro. The only constructors which don't correspond directly to constructs with `Session`
/// implementations are [`Target::Then`], which translates to a type-level function invocation to
/// concatenate two session types, and [`Target::Type`], which translates to an embedding of some
/// arbitrary session type by name (i.e. defined elsewhere as a synonym).
#[derive(Clone, Debug)]
pub enum Target {
    /// Session type: `Done`.
    Done,
    /// Session type: `Recv<T, P>`.
    Recv(Type, Rc<Spanned<Target>>),
    /// Session type: `Send<T, P>`.
    Send(Type, Rc<Spanned<Target>>),
    /// Session type: `Choose<(P, ...)>`.
    Choose(Vec<Spanned<Target>>),
    /// Session type: `Offer<(P, ...)>`.
    Offer(Vec<Spanned<Target>>),
    /// Session type: `Loop<...>`.
    Loop(Rc<Spanned<Target>>),
    /// Session type: `Continue<_N>`.
    Continue(usize),
    /// Session type: `Split<P, Q, R>`.
    Split {
        /// The transmit-only half.
        tx_only: Rc<Spanned<Target>>,
        /// The receive-only half.
        rx_only: Rc<Spanned<Target>>,
        /// The continuation.
        cont: Rc<Spanned<Target>>,
    },
    /// Session type: `Call<P, Q>`.
    Call(Rc<Spanned<Target>>, Rc<Spanned<Target>>),
    /// Session type: `<P as Then<Q>>::Combined`.
    Then(Rc<Spanned<Target>>, Rc<Spanned<Target>>),
    /// Some arbitrary session type referenced by name.
    Type(Type),
}

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Target::*;
        match self {
            Done => write!(f, "Done")?,
            Recv(t, s) => write!(f, "Recv<{}, {}>", t.to_token_stream(), s)?,
            Send(t, s) => write!(f, "Send<{}, {}>", t.to_token_stream(), s)?,
            Loop(s) => write!(f, "Loop<{}>", s)?,
            Split {
                tx_only: s,
                rx_only: p,
                cont: q,
            } => write!(f, "Split<{}, {}, {}>", s, p, q)?,
            Call(s, p) => write!(f, "Call<{}, {}>", s, p)?,
            Then(s, p) => write!(f, "<{} as Then<{}>>::Combined", s, p)?,
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
            Type(s) => write!(f, "{}", s.to_token_stream())?,
        }
        Ok(())
    }
}

impl ToTokens for Spanned<Target> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        use Target::*;

        lazy_static! {
            static ref CRATE_NAME: String = proc_macro_crate::crate_name("dialectic")
                .unwrap_or_else(|_| "dialectic".to_owned());
        }

        // We assign the associated span of the target to any type tokens generated, in an attempt
        // to get at least some type errors/issues to show up in the right place.
        let span = self.span;
        let dialectic_crate = Ident::new(&**CRATE_NAME, span);

        match &self.inner {
            Done => quote_spanned! {span=> #dialectic_crate::types::Done }.to_tokens(tokens),
            Recv(t, s) => {
                quote_spanned!(span=> #dialectic_crate::types::Recv<#t, #s>).to_tokens(tokens)
            }
            Send(t, s) => {
                quote_spanned!(span=> #dialectic_crate::types::Send<#t, #s>).to_tokens(tokens)
            }
            Loop(s) => quote_spanned!(span=> #dialectic_crate::types::Loop<#s>).to_tokens(tokens),
            Split {
                tx_only: s,
                rx_only: p,
                cont: q,
            } => {
                quote_spanned!(span=> #dialectic_crate::types::Split<#s, #p, #q>).to_tokens(tokens)
            }
            Call(s, p) => {
                quote_spanned!(span=> #dialectic_crate::types::Call<#s, #p>).to_tokens(tokens)
            }
            Then(s, p) => {
                quote_spanned!(span=> <#s as #dialectic_crate::types::Then<#p>>::Combined)
                    .to_tokens(tokens)
            }
            Choose(cs) => {
                quote_spanned!(span=> #dialectic_crate::types::Choose<(#(#cs,)*)>).to_tokens(tokens)
            }
            Offer(cs) => {
                quote_spanned!(span=> #dialectic_crate::types::Offer<(#(#cs,)*)>).to_tokens(tokens)
            }
            Continue(n) => {
                if *n > 0 {
                    quote_spanned!(span=> #dialectic_crate::types::Continue<).to_tokens(tokens);
                    for _ in 0..*n {
                        quote_spanned!(span=> #dialectic_crate::unary::S< ).to_tokens(tokens);
                    }
                    quote_spanned!(span=> #dialectic_crate::unary::Z ).to_tokens(tokens);
                    for _ in 0..=*n {
                        quote_spanned!(span=> > ).to_tokens(tokens)
                    }
                } else {
                    quote_spanned!(span=> #dialectic_crate::types::Continue).to_tokens(tokens);
                }
            }
            Type(s) => quote_spanned!(span=> #s).to_tokens(tokens),
        }
    }
}
