pub trait Boxable {
    fn boxed(self) -> Box<Self>;
}

impl<T> Boxable for T {
    /// Convenience method that returns a `Box` containing the `Expr`.
    fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}
