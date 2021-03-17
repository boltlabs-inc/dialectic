//! The target language of the `Session!` macro, produced by the compiler.

use {
    proc_macro2::TokenStream,
    quote::{quote_spanned, ToTokens},
    std::{fmt, rc::Rc},
    syn::{Path, Type},
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
                write!(f, "Continue<{}>", n)?;
            }
            Type(s) => write!(f, "{}", s.to_token_stream())?,
        }
        Ok(())
    }
}

impl Spanned<Target> {
    /// Convert this `Spanned<Target>` into a `TokenStream` using a provided crate name when
    /// referencing types from dialectic.
    pub fn to_token_stream_with_crate_name(&self, dialectic_crate: &Path) -> TokenStream {
        let mut tokens = TokenStream::new();
        self.to_tokens_with_crate_name(dialectic_crate, &mut tokens);
        tokens
    }

    /// Convert this `Spanned<Target>` into tokens and append them to the provided `TokenStream`
    /// using a provided crate name when referencing types from dialectic.
    pub fn to_tokens_with_crate_name(&self, dialectic_crate: &Path, tokens: &mut TokenStream) {
        use Target::*;

        // We assign the associated span of the target to any type tokens generated, in an attempt
        // to get at least some type errors/issues to show up in the right place.
        let span = self.span;

        match &self.inner {
            Done => quote_spanned! {span=> #dialectic_crate::types::Done }.to_tokens(tokens),
            Recv(t, s) => {
                let s = s.to_token_stream_with_crate_name(dialectic_crate);
                quote_spanned!(span=> #dialectic_crate::types::Recv<#t, #s>).to_tokens(tokens);
            }
            Send(t, s) => {
                let s = s.to_token_stream_with_crate_name(dialectic_crate);
                quote_spanned!(span=> #dialectic_crate::types::Send<#t, #s>).to_tokens(tokens);
            }
            Loop(s) => {
                let s = s.to_token_stream_with_crate_name(dialectic_crate);
                quote_spanned!(span=> #dialectic_crate::types::Loop<#s>).to_tokens(tokens);
            }
            Split {
                tx_only: s,
                rx_only: p,
                cont: q,
            } => {
                let s = s.to_token_stream_with_crate_name(dialectic_crate);
                let p = p.to_token_stream_with_crate_name(dialectic_crate);
                let q = q.to_token_stream_with_crate_name(dialectic_crate);
                quote_spanned!(span=> #dialectic_crate::types::Split<#s, #p, #q>).to_tokens(tokens);
            }
            Call(s, p) => {
                let s = s.to_token_stream_with_crate_name(dialectic_crate);
                let p = p.to_token_stream_with_crate_name(dialectic_crate);
                quote_spanned!(span=> #dialectic_crate::types::Call<#s, #p>).to_tokens(tokens);
            }
            Then(s, p) => {
                let s = s.to_token_stream_with_crate_name(dialectic_crate);
                let p = p.to_token_stream_with_crate_name(dialectic_crate);
                quote_spanned!(span=> <#s as #dialectic_crate::types::Then<#p>>::Combined)
                    .to_tokens(tokens);
            }
            Choose(cs) => {
                let cs = cs
                    .iter()
                    .map(|c| c.to_token_stream_with_crate_name(dialectic_crate));
                quote_spanned!(span=> #dialectic_crate::types::Choose<(#(#cs,)*)>).to_tokens(tokens)
            }
            Offer(cs) => {
                let cs = cs
                    .iter()
                    .map(|c| c.to_token_stream_with_crate_name(dialectic_crate));
                quote_spanned!(span=> #dialectic_crate::types::Offer<(#(#cs,)*)>).to_tokens(tokens)
            }
            Continue(n) => {
                quote_spanned!(span=> #dialectic_crate::types::Continue<#n>).to_tokens(tokens)
            }
            Type(s) => quote_spanned!(span=> #s).to_tokens(tokens),
        }
    }
}

impl ToTokens for Spanned<Target> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.to_tokens_with_crate_name(&crate::dialectic_path(), tokens);
    }
}
