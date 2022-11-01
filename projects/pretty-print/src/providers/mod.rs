use crate::{helpers::PrettySequence, PrettyPrint, PrettyTree};
use alloc::{borrow::Cow, rc::Rc};
use color_ansi::AnsiStyle;
use core::fmt::{Debug, Formatter};

/// Represents a pretty-printable tree provider.
pub struct PrettyProvider {
    width: usize,
    // arena: Arena<ColorSpec>,
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
    interface: Rc<AnsiStyle>,
}

impl<'a> Debug for PrettyProvider {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("PrettyProvider").finish()
    }
}

impl PrettyProvider {
    /// Creates a new pretty-printable tree provider.
    pub fn new(width: usize) -> Self {
        PrettyProvider {
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
            interface: AnsiStyle::rgb(197, 119, 207).into(),
        }
    }
}

impl PrettyProvider {
    pub fn get_width(&self) -> usize {
        self.width
    }
    pub fn set_width(&mut self, width: usize) {
        self.width = width;
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
    pub fn interface<S>(&self, text: S) -> PrettyTree
    where
        S: Into<Cow<'static, str>>,
    {
        PrettyTree::text(text).annotate(self.interface.clone())
    }
}

impl PrettyProvider {
    pub fn join<I, T>(&self, iter: &[I], joint: T) -> PrettyTree
    where
        I: PrettyPrint,
        T: PrettyPrint,
    {
        let mut iters = iter.iter().map(|x| x.pretty(self));
        let mut terms = PrettySequence::new(iter.len() * 2);
        terms += iters.next().unwrap_or(PrettyTree::Nil);
        for term in iters {
            terms += joint.pretty(self);
            terms += term;
        }
        terms.into()
    }
    pub fn concat<T>(&self, iter: &[T]) -> PrettyTree
    where
        T: PrettyPrint,
    {
        let mut out = PrettyTree::Nil;
        for term in iter {
            out = out.append(term.pretty(self));
        }
        out
    }
}
