use crate::{PrettyPrint, PrettyTree};
use alloc::{borrow::Cow, rc::Rc};
use color_ansi::AnsiStyle;
use core::fmt::{Debug, Formatter};

/// Represents a pretty-printable tree provider.
pub struct PrettyProvider {
    width: usize,
    keyword: Rc<AnsiStyle>,
    string: Rc<AnsiStyle>,
    number: Rc<AnsiStyle>,
    macros: Rc<AnsiStyle>,
    argument: Rc<AnsiStyle>,
    argument_mut: Rc<AnsiStyle>,
    local: Rc<AnsiStyle>,
    local_mut: Rc<AnsiStyle>,
    operator: Rc<AnsiStyle>,
    structure: Rc<AnsiStyle>,
    variant: Rc<AnsiStyle>,
    interface: Rc<AnsiStyle>,
}

impl Debug for PrettyProvider {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("PrettyProvider").finish()
    }
}

impl PrettyProvider {
    /// Creates a new pretty-printable tree provider.
    pub fn new(width: usize) -> Self {
        Self {
            width,
            keyword: AnsiStyle::rgb(197, 119, 207).into(),
            string: AnsiStyle::rgb(152, 195, 121).into(),
            number: AnsiStyle::rgb(206, 153, 100).into(),
            macros: AnsiStyle::rgb(87, 182, 194).into(),
            argument: AnsiStyle::rgb(239, 112, 117).into(),
            argument_mut: AnsiStyle::rgb(239, 112, 117).with_underline().into(),
            local: AnsiStyle::rgb(152, 195, 121).into(),
            local_mut: AnsiStyle::rgb(152, 195, 121).with_underline().into(),
            operator: AnsiStyle::rgb(90, 173, 238).into(),
            structure: AnsiStyle::rgb(197, 119, 207).into(),
            variant: AnsiStyle::rgb(239, 112, 117).into(),
            interface: AnsiStyle::rgb(197, 119, 207).into(),
        }
    }
}

impl PrettyProvider {
    /// Gets the width of the document.
    pub fn get_width(&self) -> usize {
        self.width
    }
    /// Sets the width of the document.
    pub fn set_width(&mut self, width: usize) {
        self.width = width;
    }
    /// Gets the width of the document.
    pub fn text<S>(&self, text: S) -> PrettyTree
    where
        S: Into<Cow<'static, str>>,
    {
        PrettyTree::text(text)
    }
    /// Gets the width of the document.
    pub fn custom<S>(&self, text: S, style: Rc<AnsiStyle>) -> PrettyTree
    where
        S: Into<Cow<'static, str>>,
    {
        PrettyTree::text(text).annotate(style)
    }
    /// Allocate a document containing the given text.
    pub fn keyword<S>(&self, text: S) -> PrettyTree
    where
        S: Into<Cow<'static, str>>,
    {
        PrettyTree::text(text).annotate(self.keyword.clone())
    }
    /// Allocate a document containing the given text.
    pub fn identifier<S>(&self, text: S) -> PrettyTree
    where
        S: Into<Cow<'static, str>>,
    {
        PrettyTree::text(text).annotate(self.operator.clone())
    }
    /// Allocate a document containing the given text.
    pub fn generic<S>(&self, text: S) -> PrettyTree
    where
        S: Into<Cow<'static, str>>,
    {
        PrettyTree::text(text).annotate(self.macros.clone())
    }

    /// Allocate a document containing the given text.
    pub fn variable<S>(&self, text: S, mutable: bool) -> PrettyTree
    where
        S: Into<Cow<'static, str>>,
    {
        if mutable {
            PrettyTree::text(text).annotate(self.local_mut.clone())
        }
        else {
            PrettyTree::text(text).annotate(self.local.clone())
        }
    }
    /// Allocate a document containing the given text.
    pub fn argument<S>(&self, text: S, mutable: bool) -> PrettyTree
    where
        S: Into<Cow<'static, str>>,
    {
        if mutable {
            PrettyTree::text(text).annotate(self.argument_mut.clone())
        }
        else {
            PrettyTree::text(text).annotate(self.argument.clone())
        }
    }
    /// Allocate a document containing the given text.
    pub fn operator<S>(&self, text: S) -> PrettyTree
    where
        S: Into<Cow<'static, str>>,
    {
        PrettyTree::text(text).annotate(self.operator.clone())
    }
    /// Allocate a document containing the given text.
    pub fn string<S>(&self, text: S) -> PrettyTree
    where
        S: Into<Cow<'static, str>>,
    {
        PrettyTree::text(text).annotate(self.string.clone())
    }
    /// Allocate a document containing the given text.
    pub fn annotation<S>(&self, text: S) -> PrettyTree
    where
        S: Into<Cow<'static, str>>,
    {
        PrettyTree::text(text).annotate(self.macros.clone())
    }
    /// Allocate a document containing the given text.
    pub fn number<S>(&self, text: S) -> PrettyTree
    where
        S: Into<Cow<'static, str>>,
    {
        PrettyTree::text(text).annotate(self.number.clone())
    }
    /// Allocate a document containing the given text.
    pub fn structure<S>(&self, text: S) -> PrettyTree
    where
        S: Into<Cow<'static, str>>,
    {
        PrettyTree::text(text).annotate(self.structure.clone())
    }
    /// Allocate a document containing the given text.
    pub fn variant<S>(&self, text: S) -> PrettyTree
    where
        S: Into<Cow<'static, str>>,
    {
        PrettyTree::text(text).annotate(self.variant.clone())
    }

    /// Allocate a document containing the given text.
    pub fn interface<S>(&self, text: S) -> PrettyTree
    where
        S: Into<Cow<'static, str>>,
    {
        PrettyTree::text(text).annotate(self.interface.clone())
    }
}

impl PrettyProvider {
    /// Allocate a document containing the given text.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pretty_print::PrettyProvider;
    /// let theme = PrettyProvider::new(80);
    /// theme.join(vec!["a", "b", "c"], ", ");
    /// ```
    pub fn join<I, T1, T2>(&self, iter: I, joint: T2) -> PrettyTree
    where
        I: IntoIterator<Item = T1>,
        T1: PrettyPrint,
        T2: PrettyPrint,
    {
        PrettyTree::join(iter.into_iter().map(|x| x.pretty(self)), joint.pretty(self))
    }
    /// Allocate a document containing the given text.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pretty_print::PrettyProvider;
    /// let theme = PrettyProvider::new(80);
    /// theme.join(&["a", "b", "c"], ", ");
    /// ```
    pub fn join_slice<I, T>(&self, iter: &[I], joint: T) -> PrettyTree
    where
        I: PrettyPrint,
        T: PrettyPrint,
    {
        PrettyTree::join(iter.iter().map(|s| s.pretty(self)), joint.pretty(self))
    }
    /// Allocate a document containing the given text.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pretty_print::PrettyProvider;
    /// let theme = PrettyProvider::new(80);
    /// theme.concat(vec!["1", "2", "3"]);
    /// ```
    pub fn concat<I, T>(&self, iter: I) -> PrettyTree
    where
        I: IntoIterator<Item = T>,
        T: PrettyPrint,
    {
        PrettyTree::concat(iter.into_iter().map(|x| x.pretty(self)))
    }
    /// Allocate a document containing the given text.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pretty_print::PrettyProvider;
    /// let theme = PrettyProvider::new(80);
    /// theme.concat_slice(&["1", "2", "3"]);
    /// ```
    pub fn concat_slice<T>(&self, iter: &[T]) -> PrettyTree
    where
        T: PrettyPrint,
    {
        PrettyTree::concat(iter.iter().map(|s| s.pretty(self)))
    }
}
