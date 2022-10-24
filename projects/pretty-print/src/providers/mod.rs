use crate::{DocumentTree, PrettyPrint};
use alloc::borrow::Cow;
use core::fmt::{Debug, Formatter};
use termcolor::{Color, ColorSpec};

/// Represents a pretty-printable tree provider.
pub struct PrettyProvider {
    // arena: Arena<ColorSpec>,
    keyword: ColorSpec,
    string: ColorSpec,
    number: ColorSpec,
    macros: ColorSpec,
    argument: ColorSpec,
    argument_mut: ColorSpec,
    local: ColorSpec,
    local_mut: ColorSpec,
    operator: ColorSpec,
    structure: ColorSpec,
    interface: ColorSpec,
}

impl<'a> Debug for PrettyProvider {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("PrettyProvider").finish()
    }
}

impl PrettyProvider {
    /// Creates a new pretty-printable tree provider.
    pub fn new() -> Self {
        let argument = Color::Rgb(239, 112, 117);
        let purple = Color::Rgb(197, 119, 207);
        let local = Color::Rgb(152, 195, 121);
        let green = Color::Rgb(152, 195, 121);
        PrettyProvider {
            keyword: ColorSpec::new().set_fg(Some(purple)).clone(),
            string: ColorSpec::new().set_fg(Some(green)).clone(),
            number: ColorSpec::new().set_fg(Some(Color::Rgb(206, 153, 100))).clone(),
            macros: ColorSpec::new().set_fg(Some(Color::Rgb(87, 182, 194))).clone(),
            argument: ColorSpec::new().set_fg(Some(argument)).clone(),
            argument_mut: ColorSpec::new().set_fg(Some(argument)).set_underline(true).clone(),
            local: ColorSpec::new().set_fg(Some(local)).clone(),
            local_mut: ColorSpec::new().set_fg(Some(local)).set_underline(true).clone(),
            operator: ColorSpec::new().set_fg(Some(Color::Rgb(90, 173, 238))).clone(),
            structure: ColorSpec::new().set_fg(Some(Color::Rgb(197, 119, 207))).clone(),
            interface: ColorSpec::new().set_fg(Some(Color::Rgb(197, 119, 207))).clone(),
        }
    }
}

impl PrettyProvider {
    /// Allocate a document containing the given text.
    pub fn keyword<S>(&self, text: S) -> DocumentTree
    where
        S: Into<Cow<'static, str>>,
    {
        DocumentTree::text(text).annotate(self.keyword.clone())
    }
    /// Allocate a document containing the given text.
    pub fn identifier<S>(&self, text: S) -> DocumentTree
    where
        S: Into<Cow<'static, str>>,
    {
        DocumentTree::text(text).annotate(self.keyword.clone())
    }
    /// Allocate a document containing the given text.
    pub fn generic<S>(&self, text: S) -> DocumentTree
    where
        S: Into<Cow<'static, str>>,
    {
        DocumentTree::text(text).annotate(self.keyword.clone())
    }

    /// Allocate a document containing the given text.
    pub fn variable<S>(&self, text: S, mutable: bool) -> DocumentTree
    where
        S: Into<Cow<'static, str>>,
    {
        DocumentTree::text(text).annotate(self.keyword.clone())
    }

    /// Allocate a document containing the given text.
    pub fn argument<S>(&self, text: S, mutable: bool) -> DocumentTree
    where
        S: Into<Cow<'static, str>>,
    {
        DocumentTree::text(text).annotate(self.keyword.clone())
    }

    /// Allocate a document containing the given text.
    pub fn operator<S>(&self, text: S) -> DocumentTree
    where
        S: Into<Cow<'static, str>>,
    {
        DocumentTree::text(text).annotate(self.keyword.clone())
    }
    /// Allocate a document containing the given text.
    pub fn string<S>(&self, text: S) -> DocumentTree
    where
        S: Into<Cow<'static, str>>,
    {
        DocumentTree::text(text).annotate(self.keyword.clone())
    }
    /// Allocate a document containing the given text.
    pub fn metadata<S>(&self, text: S) -> DocumentTree
    where
        S: Into<Cow<'static, str>>,
    {
        DocumentTree::text(text).annotate(self.keyword.clone())
    }
    /// Allocate a document containing the given text.
    pub fn number<S>(&self, text: S) -> DocumentTree
    where
        S: Into<Cow<'static, str>>,
    {
        DocumentTree::text(text).annotate(self.keyword.clone())
    }
    /// Allocate a document containing the given text.
    pub fn structure<S>(&self, text: S) -> DocumentTree
    where
        S: Into<Cow<'static, str>>,
    {
        DocumentTree::text(text).annotate(self.keyword.clone())
    }
    /// Allocate a document containing the given text.
    pub fn interface<S>(&self, text: S) -> DocumentTree
    where
        S: Into<Cow<'static, str>>,
    {
        DocumentTree::text(text).annotate(self.keyword.clone())
    }
}

impl<'a> PrettyProvider {
    /// Allocate a document that intersperses the given separator `S` between the given documents
    /// `[A, B, C, ..., Z]`, yielding `[A, S, B, S, C, S, ..., S, Z]`.
    ///
    /// Compare [the `intersperse` method from the `itertools` crate](https://docs.rs/itertools/0.5.9/itertools/trait.Itertools.html#method.intersperse).
    ///
    /// NOTE: The separator type, `S` may need to be cloned. Consider using cheaply cloneable ptr
    /// like `RefDoc` or `RcDoc`
    #[inline]
    pub fn intersperse<T, S>(&'a self, terms: &[T], joint: S) -> DocumentTree
    where
        T: PrettyPrint,
    {
        todo!()
    }
    /// Allocate a document that intersperses the given separator `S` between the given documents
    /// `[A, B, C, ..., Z]`, yielding `[A, S, B, S, C, S, ..., S, Z]`.
    ///
    /// Compare [the `intersperse` method from the `itertools` crate](https://docs.rs/itertools/0.5.9/itertools/trait.Itertools.html#method.intersperse).
    ///
    /// NOTE: The separator type, `S` may need to be cloned. Consider using cheaply cloneable ptr
    /// like `RefDoc` or `RcDoc`
    #[inline]
    pub fn join<T>(&'a self, terms: &[T], joint: &'static str) -> DocumentTree
    where
        T: PrettyPrint,
    {
        todo!()
    }
}
