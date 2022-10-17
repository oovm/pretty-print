use crate::{Pretty, PrettyPrint};
use alloc::borrow::Cow;
use core::fmt::{Debug, Formatter};
use termcolor::{Color, ColorSpec};
use typed_arena::Arena;
use crate::DocBuilder;

/// Represents a pretty-printable tree.
pub type PrettyTree<'a> = DocBuilder<'a, Arena<ColorSpec>, ColorSpec>;

/// Represents a pretty-printable tree provider.
pub struct PrettyProvider<'a> {
    arena: Arena<ColorSpec>,
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

impl<'a> Debug for PrettyProvider<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("PrettyProvider").finish()
    }
}

impl<'a> PrettyProvider<'a> {
    /// Creates a new pretty-printable tree provider.
    pub fn new() -> Self {
        let argument = Color::Rgb(239, 112, 117);
        let purple = Color::Rgb(197, 119, 207);
        let local = Color::Rgb(152, 195, 121);
        let green = Color::Rgb(152, 195, 121);
        PrettyProvider {
            arena: Arena::new(),
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

impl<'a> PrettyProvider<'a> {
    /// Creates a new pretty-printable tree provider.
    pub fn nil(&'a self) -> PrettyTree<'a> {
        self.arena.nil()
    }
    /// Creates a new pretty-printable tree provider.
    pub fn space(&'a self) -> PrettyTree<'a> {
        self.arena.space()
    }
    /// Creates a new pretty-printable tree provider.
    pub fn hardline(&'a self) -> PrettyTree<'a> {
        self.arena.hardline()
    }
    /// Allocate a document containing the given text.
    ///
    /// The given text must not contain line breaks.
    pub fn text<'i, S>(&'a self, text: S) -> PrettyTree<'a>
        where
            S: Into<Cow<'i, str>>,
            'i: 'a,
    {
        todo!()
        // self.arena.text(text.into())
    }
    /// Allocate a document containing the given text.
    pub fn keyword<'i, S>(&'a self, text: S) -> PrettyTree<'a>
        where
            S: Into<Cow<'i, str>>,
            'i: 'a,
    {
        self.text(text).annotate(self.keyword.clone())
    }
    /// Allocate a document containing the given text.
    pub fn identifier<'i, S>(&'a self, text: S) -> PrettyTree<'a>
        where
            S: Into<Cow<'i, str>>,
            'i: 'a,
    {
        self.operator(text)
    }
    /// Allocate a document containing the given text.
    pub fn generic<'i, S>(&'a self, text: S) -> PrettyTree<'a>
        where
            S: Into<Cow<'i, str>>,
            'i: 'a,
    {
        self.text(text).annotate(self.macros.clone())
    }

    /// Allocate a document containing the given text.
    pub fn variable<'i, S>(&'a self, text: S, mutable: bool) -> PrettyTree<'a>
        where
            S: Into<Cow<'i, str>>,
            'i: 'a,
    {
        if mutable { self.text(text).annotate(self.local_mut.clone()) } else { self.text(text).annotate(self.local.clone()) }
    }

    /// Allocate a document containing the given text.
    pub fn argument<'i, S>(&'a self, text: S, mutable: bool) -> PrettyTree<'a>
        where
            S: Into<Cow<'i, str>>,
            'i: 'a,
    {
        if mutable {
            self.text(text).annotate(self.argument_mut.clone())
        } else {
            self.text(text).annotate(self.argument.clone())
        }
    }
    /// Allocate a document containing the given text.
    pub fn operator<'i, S>(&'a self, text: S) -> PrettyTree<'a>
        where
            S: Into<Cow<'i, str>>,
            'i: 'a,
    {
        self.text(text).annotate(self.operator.clone())
    }
    /// Allocate a document containing the given text.
    pub fn string<'i, S>(&'a self, text: S) -> PrettyTree<'a>
        where
            S: Into<Cow<'i, str>>,
            'i: 'a,
    {
        self.text(text).annotate(self.string.clone())
    }
    /// Allocate a document containing the given text.
    pub fn metadata<'i, S>(&'a self, text: S) -> PrettyTree<'a>
        where
            S: Into<Cow<'i, str>>,
            'i: 'a,
    {
        self.text(text).annotate(self.macros.clone())
    }

    /// Allocate a document containing the given text.
    pub fn number<'i, S>(&'a self, text: S) -> PrettyTree<'a>
        where
            S: Into<Cow<'i, str>>,
            'i: 'a,
    {
        self.text(text).annotate(self.number.clone())
    }
    /// Allocate a document containing the given text.
    pub fn structure<'i, S>(&'a self, text: S) -> PrettyTree<'a>
        where
            S: Into<Cow<'i, str>>,
            'i: 'a,
    {
        self.text(text).annotate(self.structure.clone())
    }
    /// Allocate a document containing the given text.
    pub fn interface<'i, S>(&'a self, text: S) -> PrettyTree<'a>
        where
            S: Into<Cow<'i, str>>,
            'i: 'a,
    {
        self.text(text).annotate(self.interface.clone())
    }
}

impl<'a> PrettyProvider<'a> {
    /// Allocate a document concatenating the given documents.
    #[inline]
    pub fn concat<I>(&'a self, docs: I) -> PrettyTree<'a>
        where
            I: IntoIterator,
            I::Item: Pretty<'a, Arena<'a, ColorSpec>, ColorSpec>,
    {
        self.arena.concat(docs)
    }
    /// Allocate a document that intersperses the given separator `S` between the given documents
    /// `[A, B, C, ..., Z]`, yielding `[A, S, B, S, C, S, ..., S, Z]`.
    ///
    /// Compare [the `intersperse` method from the `itertools` crate](https://docs.rs/itertools/0.5.9/itertools/trait.Itertools.html#method.intersperse).
    ///
    /// NOTE: The separator type, `S` may need to be cloned. Consider using cheaply cloneable ptr
    /// like `RefDoc` or `RcDoc`
    #[inline]
    pub fn intersperse<T, S>(&'a self, terms: &[T], joint: S) -> PrettyTree<'a>
        where
            T: PrettyPrint,
            S: Pretty<'a, Arena<'a, ColorSpec>, ColorSpec> + Clone,
    {
        self.arena.intersperse(terms.iter().map(|x| x.build(self)), joint)
    }
    /// Allocate a document that intersperses the given separator `S` between the given documents
    /// `[A, B, C, ..., Z]`, yielding `[A, S, B, S, C, S, ..., S, Z]`.
    ///
    /// Compare [the `intersperse` method from the `itertools` crate](https://docs.rs/itertools/0.5.9/itertools/trait.Itertools.html#method.intersperse).
    ///
    /// NOTE: The separator type, `S` may need to be cloned. Consider using cheaply cloneable ptr
    /// like `RefDoc` or `RcDoc`
    #[inline]
    pub fn join<T>(&'a self, terms: &[T], joint: &'static str) -> PrettyTree<'a>
        where
            T: PrettyPrint,
    {
        self.arena.intersperse(terms.iter().map(|x| x.build(self)), self.arena.text(joint))
    }
}
