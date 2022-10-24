use crate::{DocumentTree, PrettyPrint};
use alloc::{borrow::Cow, rc::Rc};
use color_ansi::{AnsiColor, AnsiStyle};
use core::fmt::{Debug, Formatter};
/// Represents a pretty-printable tree provider.
pub struct PrettyProvider {
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
    pub fn new() -> Self {
        let argument = AnsiColor::Rgb(239, 112, 117);
        let purple = AnsiColor::Rgb(197, 119, 207);
        let local = AnsiColor::Rgb(152, 195, 121);
        let green = AnsiColor::Rgb(152, 195, 121);
        PrettyProvider {
            keyword: AnsiStyle::rgb(197, 119, 207).set_fg(Some(purple)).clone(),
            string: AnsiStyle::rgb().set_fg(Some(green)).clone(),
            number: AnsiStyle::rgb().set_fg(Some(Color::Rgb(206, 153, 100))).clone(),
            macros: AnsiStyle::rgb().set_fg(Some(Color::Rgb(87, 182, 194))).clone(),
            argument: AnsiStyle::rgb().set_fg(Some(argument)).clone(),
            argument_mut: AnsiStyle::rgb().set_fg(Some(argument)).set_underline(true).clone(),
            local: AnsiStyle::rgb().set_fg(Some(local)).clone(),
            local_mut: AnsiStyle::rgb().set_fg(Some(local)).set_underline(true).clone(),
            operator: AnsiStyle::rgb().set_fg(Some(Color::Rgb(90, 173, 238))).clone(),
            structure: AnsiStyle::rgb().set_fg(Some(Color::Rgb(197, 119, 207))).clone(),
            interface: AnsiStyle::rgb().set_fg(Some(Color::Rgb(197, 119, 207))).clone(),
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
        DocumentTree::text(text).annotate(self.operator.clone())
    }
    /// Allocate a document containing the given text.
    pub fn generic<S>(&self, text: S) -> DocumentTree
    where
        S: Into<Cow<'static, str>>,
    {
        DocumentTree::text(text).annotate(self.macros.clone())
    }

    /// Allocate a document containing the given text.
    pub fn variable<S>(&self, text: S, mutable: bool) -> DocumentTree
    where
        S: Into<Cow<'static, str>>,
    {
        if mutable {
            DocumentTree::text(text).annotate(self.local_mut.clone())
        }
        else {
            DocumentTree::text(text).annotate(self.local.clone())
        }
    }
    /// Allocate a document containing the given text.
    pub fn argument<S>(&self, text: S, mutable: bool) -> DocumentTree
    where
        S: Into<Cow<'static, str>>,
    {
        if mutable {
            DocumentTree::text(text).annotate(self.argument_mut.clone())
        }
        else {
            DocumentTree::text(text).annotate(self.argument.clone())
        }
    }
    /// Allocate a document containing the given text.
    pub fn operator<S>(&self, text: S) -> DocumentTree
    where
        S: Into<Cow<'static, str>>,
    {
        DocumentTree::text(text).annotate(self.operator.clone())
    }
    /// Allocate a document containing the given text.
    pub fn string<S>(&self, text: S) -> DocumentTree
    where
        S: Into<Cow<'static, str>>,
    {
        DocumentTree::text(text).annotate(self.string.clone())
    }
    /// Allocate a document containing the given text.
    pub fn annotation<S>(&self, text: S) -> DocumentTree
    where
        S: Into<Cow<'static, str>>,
    {
        DocumentTree::text(text).annotate(self.macros.clone())
    }
    /// Allocate a document containing the given text.
    pub fn number<S>(&self, text: S) -> DocumentTree
    where
        S: Into<Cow<'static, str>>,
    {
        DocumentTree::text(text).annotate(self.number.clone())
    }
    /// Allocate a document containing the given text.
    pub fn structure<S>(&self, text: S) -> DocumentTree
    where
        S: Into<Cow<'static, str>>,
    {
        DocumentTree::text(text).annotate(self.structure.clone())
    }
    /// Allocate a document containing the given text.
    pub fn interface<S>(&self, text: S) -> DocumentTree
    where
        S: Into<Cow<'static, str>>,
    {
        DocumentTree::text(text).annotate(self.interface.clone())
    }
}

impl PrettyProvider {
    pub fn join<I, T>(&self, iter: &[I], joint: T) -> DocumentTree
    where
        I: PrettyPrint,
        T: PrettyPrint,
    {
        let mut out = DocumentTree::Nil;
        for term in iter {
            out = out.append(term.pretty(self)).append(joint.pretty(self));
        }
        out
    }
}
