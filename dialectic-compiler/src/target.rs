use {std::rc::Rc, syn::Type};

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
