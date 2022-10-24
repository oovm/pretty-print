use super::*;
use crate::PrettyBuilder;

#[derive(Clone, Debug, Default)]
pub struct DocumentSequence {
    items: Vec<DocumentTree>,
}

impl DocumentSequence {
    pub fn new(capacity: usize) -> Self {
        Self { items: Vec::with_capacity(capacity) }
    }
    pub fn push<T>(&mut self, item: T)
    where
        T: Into<DocumentTree>,
    {
        self.items.push(item.into());
    }
    pub fn extend<I, T>(&mut self, items: I)
    where
        I: IntoIterator<Item = T>,
        T: Into<DocumentTree>,
    {
        self.items.extend(items.into_iter().map(|x| x.into()));
    }
}

impl PrettyBuilder for DocumentSequence {
    fn flat_alt<E>(self, flat: E) -> DocumentTree
    where
        E: Into<DocumentTree>,
    {
        DocumentTree::from(self).flat_alt(flat)
    }
}

impl<T> AddAssign<T> for DocumentSequence
where
    T: Into<DocumentTree>,
{
    fn add_assign(&mut self, rhs: T) {
        self.push(rhs);
    }
}

impl From<DocumentSequence> for DocumentTree {
    fn from(value: DocumentSequence) -> Self {
        Self::concat(value.items)
    }
}
