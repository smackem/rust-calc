use std::rc::Rc;

/// A trait that enables boxing of values with a fluent api, e.g. `it.collect().boxed()`.
pub trait Boxable {
    fn boxed(self) -> Box<Self>;
    fn rc(self) -> Rc<Self>;
}

impl<T> Boxable for T {
    /// Convenience method that returns a `Box` containing `self`.
    fn boxed(self) -> Box<Self> {
        Box::new(self)
    }

    fn rc(self) -> Rc<Self> {
        Rc::new(self)
    }
}
