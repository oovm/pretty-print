use super::*;

/// The document sequence type.
#[derive(Clone, Debug, Default)]
pub struct PrettySequence {
    items: Vec<PrettyTree>,
}

impl PrettySequence {
    /// Create a new sequence with the given capacity.
    pub fn new(capacity: usize) -> Self {
        Self { items: Vec::with_capacity(capacity) }
    }
    /// Create a new sequence with the given capacity.
    pub fn push<T>(&mut self, item: T)
    where
        T: Into<PrettyTree>,
    {
        self.items.push(item.into());
    }
    /// Create a new sequence with the given capacity.
    pub fn extend<I, T>(&mut self, items: I)
    where
        I: IntoIterator<Item = T>,
        T: Into<PrettyTree>,
    {
        self.items.extend(items.into_iter().map(|x| x.into()));
    }
}

impl PrettyBuilder for PrettySequence {
    fn flat_alt<E>(self, flat: E) -> PrettyTree
    where
        E: Into<PrettyTree>,
    {
        PrettyTree::from(self).flat_alt(flat)
    }
    fn indent(self, indent: usize) -> PrettyTree {
        PrettyTree::from(self).indent(indent)
    }

    fn nest(self, offset: isize) -> PrettyTree {
        PrettyTree::from(self).nest(offset)
    }
}

impl<T> AddAssign<T> for PrettySequence
where
    T: Into<PrettyTree>,
{
    fn add_assign(&mut self, rhs: T) {
        self.push(rhs);
    }
}

impl From<PrettySequence> for PrettyTree {
    fn from(value: PrettySequence) -> Self {
        Self::concat(value.items)
    }
}
