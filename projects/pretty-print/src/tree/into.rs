use super::*;

impl<T> Add<T> for PrettyTree
where
    T: Into<Self>,
{
    type Output = Self;
    fn add(self, rhs: T) -> Self::Output {
        self.append(rhs.into())
    }
}

impl<T> AddAssign<T> for PrettyTree
where
    T: Into<Self>,
{
    fn add_assign(&mut self, rhs: T) {
        *self = self.clone().append(rhs.into());
    }
}

impl<T> From<Option<T>> for PrettyTree
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

impl From<()> for PrettyTree {
    fn from(_: ()) -> Self {
        Self::Nil
    }
}

impl From<&'static str> for PrettyTree {
    fn from(s: &'static str) -> Self {
        Self::StaticText(s)
    }
}

impl From<String> for PrettyTree {
    fn from(s: String) -> Self {
        Self::Text(Rc::from(s))
    }
}
