//! The target language of the `Session!` macro, produced by the compiler.

use {
    lazy_static::lazy_static,
    proc_macro2::{Span, TokenStream},
    quote::{quote, ToTokens},
    std::{fmt, rc::Rc},
    syn::{Ident, Type},
};

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
    Recv(Type, Rc<Target>),
    /// Session type: `Send<T, P>`.
    Send(Type, Rc<Target>),
    /// Session type: `Choose<(P, ...)>`.
    Choose(Vec<Target>),
    /// Session type: `Offer<(P, ...)>`.
    Offer(Vec<Target>),
    /// Session type: `Loop<...>`.
    Loop(Rc<Target>),
    /// Session type: `Continue<_N>`.
    Continue(usize),
    /// Session type: `Split<P, Q, R>`.
    Split(Rc<Target>, Rc<Target>, Rc<Target>),
    /// Session type: `Call<P, Q>`.
    Call(Rc<Target>, Rc<Target>),
    /// Session type: `<P as Then<Q>>::Combined`.
    Then(Rc<Target>, Rc<Target>),
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
            Split(s, p, q) => write!(f, "Split<{}, {}, {}>", s, p, q)?,
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

impl ToTokens for Target {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        use Target::*;

        lazy_static! {
            static ref CRATE_NAME: String = proc_macro_crate::crate_name("dialectic")
                .unwrap_or_else(|_| "dialectic".to_owned());
        }

        let c = Ident::new(&**CRATE_NAME, Span::call_site());

        match self {
            Done => quote! { #c::types::Done }.to_tokens(tokens),
            Recv(t, s) => quote! { #c::types::Recv<#t, #s> }.to_tokens(tokens),
            Send(t, s) => quote! { #c::types::Send<#t, #s> }.to_tokens(tokens),
            Loop(s) => quote! { #c::types::Loop<#s> }.to_tokens(tokens),
            Split(s, p, q) => quote! { #c::types::Split<#s, #p, #q> }.to_tokens(tokens),
            Call(s, p) => quote! { #c::types::Call<#s, #p> }.to_tokens(tokens),
            Then(s, p) => quote! { <#s as #c::types::Then<#p>>::Combined }.to_tokens(tokens),
            Choose(cs) => quote! { #c::types::Choose<(#(#cs,)*)> }.to_tokens(tokens),
            Offer(cs) => quote! { #c::types::Offer<(#(#cs,)*)> }.to_tokens(tokens),
            Continue(n) => {
                if *n > 0 {
                    (quote! { #c::types::Continue< }).to_tokens(tokens);
                    (0..*n).for_each(|_| (quote! { #c::unary::S< }).to_tokens(tokens));
                    (quote! { #c::unary::Z }).to_tokens(tokens);
                    (0..=*n).for_each(|_| (quote! { > }).to_tokens(tokens));
                } else {
                    (quote! { #c::types::Continue }).to_tokens(tokens);
                }
            }
            Type(s) => quote! { #s }.to_tokens(tokens),
        }
    }
}
