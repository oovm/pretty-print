
use crate::IdentifierNode;
use alloc::{borrow::Cow, string::String};
use core::{
    borrow::Cow,
    ops::{Deref, Range},
    string::String,
};
use pretty::{
    termcolor::{Buffer, Color, ColorSpec},
    Arena, DocAllocator, DocBuilder, Pretty,
};

pub type PrettyTree<'a> = DocBuilder<'a, Arena<'a, ColorSpec>, ColorSpec>;


pub struct PrettyProvider<'a> {
    arena: Arena<'a, ColorSpec>,
}

impl<'a> PrettyProvider<'a> {
    pub fn new() -> Self {
        PrettyProvider { arena: Arena::new() }
    }
}

impl<'a> PrettyProvider<'a> {
    pub fn nil(&'a self) -> PrettyTree<'a> {
        self.arena.nil()
    }
    pub fn space(&'a self) -> PrettyTree<'a> {
        self.arena.space()
    }
    pub fn hardline(&'a self) -> PrettyTree<'a> {
        self.arena.hardline()
    }
    pub fn text<S>(&'a self, text: S) -> PrettyTree<'a>
        where
            S: Into<Cow<'static, str>>,
    {
        self.arena.text(text)
    }
    pub(crate) fn keyword<S>(&'a self, text: S) -> PrettyTree<'a>
        where
            S: Into<Cow<'static, str>>,
    {
        let kw = ColorSpec::new().set_fg(Some(Color::Rgb(197, 119, 207))).clone();
        self.text(text.into()).annotate(kw)
    }
    pub(crate) fn identifier<S>(&'a self, text: S) -> PrettyTree<'a>
        where
            S: Into<Cow<'static, str>>,
    {
        self.operator(text)
    }
    pub(crate) fn generic<S>(&'a self, text: S) -> PrettyTree<'a>
        where
            S: Into<Cow<'static, str>>,
    {
        self.text(text.into()).annotate(self.macro_style())
    }
    pub(crate) fn argument<S>(&'a self, text: S) -> PrettyTree<'a>
        where
            S: Into<Cow<'static, str>>,
    {
        let kw = ColorSpec::new().set_fg(Some(Color::Rgb(239, 112, 117))).clone();
        self.text(text.into()).annotate(kw)
    }
    pub(crate) fn operator<S>(&'a self, text: S) -> PrettyTree<'a>
        where
            S: Into<Cow<'static, str>>,
    {
        let kw = ColorSpec::new().set_fg(Some(Color::Rgb(90, 173, 238))).clone();
        self.text(text.into()).annotate(kw)
    }

    pub(crate) fn namepath(&'a self, path: &[IdentifierNode]) -> PrettyTree<'a> {
        self.intersperse(path.iter().map(|x| self.identifier(x.name.clone())), self.text("::"))
    }
    pub(crate) fn string_style(&self) -> ColorSpec {
        ColorSpec::new().set_fg(Some(Color::Rgb(152, 195, 121))).clone()
    }
    pub(crate) fn number_style(&self) -> ColorSpec {
        ColorSpec::new().set_fg(Some(Color::Rgb(206, 153, 100))).clone()
    }
    pub(crate) fn macro_style(&self) -> ColorSpec {
        ColorSpec::new().set_fg(Some(Color::Rgb(87, 182, 194))).clone()
    }
}

impl<'a> PrettyProvider<'a> {
    #[inline]
    pub fn concat<I>(&'a self, docs: I) -> PrettyTree<'a>
        where
            I: IntoIterator,
            I::Item: Pretty<'a, Arena<'a, ColorSpec>, ColorSpec>,
    {
        self.arena.concat(docs.iter().cloned())
    }
    #[inline]
    pub fn intersperse<T, S>(&'a self, terms: &[T], joint: S) -> PrettyTree<'a>
        where
            T: PrettyPrint,
            S: Pretty<'a, Arena<'a, ColorSpec>, ColorSpec> + Clone,
    {
        self.arena.intersperse(terms.iter().map(|x| x.build(self)), joint)
    }
    #[inline]
    pub fn join<T, S>(&'a self, terms: &[T], joint: &'static str) -> PrettyTree<'a>
        where
            T: PrettyPrint,
            S: Pretty<'a, Arena<'a, ColorSpec>, ColorSpec> + Clone,
    {
        self.arena.intersperse(terms.iter().map(|x| x.build(self)), self.arena.text(joint))
    }
}
