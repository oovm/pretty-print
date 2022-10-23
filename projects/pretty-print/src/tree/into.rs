use super::*;


impl<T> Add<T> for DocumentTree
    where
        T: Into<Self>,
{
    type Output = Self;
    fn add(self, rhs: T) -> Self::Output {
        self.append(rhs.into())
    }
}

impl<T> AddAssign<T> for DocumentTree
    where
        T: Into<Self>,
{
    fn add_assign(&mut self, rhs: T) {
        *self = self.clone().append(rhs.into());
    }
}

impl<T> From<Option<T>> for DocumentTree
    where
        Self: From<T>,
{
    fn from(x: Option<T>) -> Self {
        match x {
            Some(x) => x.into(),
            None => Self::Nil,
        }
    }
}

impl From<()> for DocumentTree {
    fn from(_: ()) -> Self {
        Self::Nil
    }
}

impl From<&'static str> for DocumentTree {
    fn from(s: &'static str) -> Self {
        Self::StaticText(s)
    }
}

impl From<String> for DocumentTree {
    fn from(s: String) -> Self {
        Self::Text(Rc::from(s))
    }
}

impl From<DocumentSequence> for DocumentTree {
    fn from(value: DocumentSequence) -> Self {
        Self::concat(value.items)
    }
}
