use {
    lazy_static::lazy_static,
    proc_macro2::{Span, TokenStream},
    quote::{quote, ToTokens},
    std::{fmt, rc::Rc},
    syn::{Ident, Type},
};

#[derive(Clone, Debug)]
pub enum Target {
    Done,
    Recv(Type, Rc<Target>),
    Send(Type, Rc<Target>),
    Choose(Vec<Target>),
    Offer(Vec<Target>),
    Loop(Rc<Target>),
    Continue(usize),
    Split(Rc<Target>, Rc<Target>, Rc<Target>),
    Call(Rc<Target>, Rc<Target>),
    Then(Rc<Target>, Rc<Target>),
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
                    (0..*n).for_each(|_| (quote! { #c::types::S< }).to_tokens(tokens));
                    (quote! { #c::types::Z }).to_tokens(tokens);
                    (0..=*n).for_each(|_| (quote! { > }).to_tokens(tokens));
                } else {
                    (quote! { #c::types::Continue }).to_tokens(tokens);
                }
            }
            Type(s) => quote! { #s }.to_tokens(tokens),
        }
    }
}
