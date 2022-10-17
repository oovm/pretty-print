// #![cfg_attr(not(feature = "std"), no_std)]
// #![deny(missing_debug_implementations, missing_copy_implementations)]
// #![warn(missing_docs, rustdoc::missing_crate_level_docs)]
#![doc = include_str!("../readme.md")]
#![doc(html_logo_url = "https://raw.githubusercontent.com/oovm/shape-rs/dev/projects/images/Trapezohedron.svg")]
#![doc(html_favicon_url = "https://raw.githubusercontent.com/oovm/shape-rs/dev/projects/images/Trapezohedron.svg")]

extern crate alloc;
extern crate core;

mod blocks;
mod providers;
mod traits;

use core::fmt::{Debug, Formatter};
pub use crate::{
    providers::{PrettyProvider, PrettyTree},
    traits::PrettyPrint,
    blocks::k_and_r_bracket::KAndRBracket,
};

use std::{
    borrow::Cow,
    convert::TryInto,
    ops::{Add, AddAssign, Deref},
    rc::Rc,
};
use std::fmt::Display;

use termcolor::{ColorSpec, WriteColor};

// pub mod block;
mod render;

// pub use self::block::{Affixes, BlockDoc};

pub use self::render::TermColored;
pub use self::render::{FmtWrite, IoWrite, Render, RenderAnnotated};

/// The concrete document type. This type is not meant to be used directly. Instead use the static
/// functions on `Doc` or the methods on an `DocAllocator`.
///
/// The `T` parameter is used to abstract over pointers to `Doc`. See `RefDoc` and `BoxDoc` for how
/// it is used
#[derive(Clone)]
pub enum DocumentTree
{
    Nil,
    Append {
        base: Rc<Self>,
        rest: Rc<Self>,
    },
    Group {
        items: Rc<Self>,
    },
    FlatAlt {
        flat: Rc<Self>,
        alt: Rc<Self>,
    },
    Nest {
        space: isize,
        doc: Rc<Self>,
    },
    Hardline,
    // Stores the length of a string document that is not just ascii
    RenderLen {
        len: usize,
        doc: Rc<Self>,
    },
    Text(Rc<str>),
    StaticText(&'static str),
    Annotated {
        color: ColorSpec,
        doc: Rc<Self>,
    },
    Union {
        left: Rc<Self>,
        right: Rc<Self>,
    },
    Column {
        column: fn(usize) -> Self,
    },
    Nesting {
        nesting: fn(usize) -> Self,
    },
    Fail,
}

impl Default for DocumentTree
{
    fn default() -> Self {
        Self::Nil
    }
}

fn append_docs<'a, 'd, T, A>(
    mut doc: &'d DocumentTree,
    consumer: &mut impl FnMut(&'d DocumentTree),
)
{
    loop {
        match doc {
            DocumentTree::Append(l, r) => {
                append_docs(l, consumer);
                doc = r;
            }
            _ => break consumer(doc),
        }
    }
}

impl Debug for DocumentTree
{
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        let is_line = |doc: &DocumentTree| match doc {
            DocumentTree::FlatAlt(x, y) => {
                matches!((&**x, &**y), (DocumentTree::Hardline, DocumentTree::StaticText(" ")))
            }
            _ => false,
        };
        let is_line_ = |doc: &DocumentTree| match doc {
            DocumentTree::FlatAlt(x, y) => {
                matches!((&**x, &**y), (DocumentTree::Hardline, DocumentTree::Nil))
            }
            _ => false,
        };
        match self {
            DocumentTree::Nil => f.debug_tuple("Nil").finish(),
            DocumentTree::Append(..) => {
                let mut f = f.debug_list();
                append_docs(self, &mut |doc| {
                    f.entry(doc);
                });
                f.finish()
            }
            _ if is_line(self) => f.debug_tuple("Line").finish(),
            _ if is_line_(self) => f.debug_tuple("Line_").finish(),
            DocumentTree::FlatAlt(ref x, ref y) => f.debug_tuple("FlatAlt").field(x).field(y).finish(),
            DocumentTree::Group(ref doc) => {
                if is_line(self) {
                    return f.debug_tuple("SoftLine").finish();
                }
                if is_line_(self) {
                    return f.debug_tuple("SoftLine_").finish();
                }
                f.debug_tuple("Group").field(doc).finish()
            }
            DocumentTree::Nest(off, ref doc) => f.debug_tuple("Nest").field(&off).field(doc).finish(),
            DocumentTree::Hardline => f.debug_tuple("Hardline").finish(),
            DocumentTree::RenderLen(_, d) => d.fmt(f),
            DocumentTree::Text(ref s) => s.fmt(f),
            DocumentTree::StaticText(ref s) => s.fmt(f),
            // Doc::SmallText(ref s) => s.fmt(f),
            DocumentTree::Annotated(ref ann, ref doc) => {
                f.debug_tuple("Annotated").field(ann).field(doc).finish()
            }
            DocumentTree::Union(ref l, ref r) => f.debug_tuple("Union").field(l).field(r).finish(),
            DocumentTree::Column(_) => f.debug_tuple("Column(..)").finish(),
            DocumentTree::Nesting(_) => f.debug_tuple("Nesting(..)").finish(),
            DocumentTree::Fail => f.debug_tuple("Fail").finish(),
        }
    }
}

impl DocumentTree {
    #[inline]
    pub fn space() -> Self {
        DocumentTree::StaticText(" ").into()
    }
}

impl DocumentTree
{
    ///   A line acts like a  `\n`  but behaves like  `space`  if it is grouped on a single line.
    #[inline]
    pub fn line() -> Self {
        Self::Hardline.flat_alt(Self::space()).into()
    }

    ///   Acts like  `line`  but behaves like  `nil`  if grouped on a single line
    #[inline]
    pub fn line_() -> Self {
        Self::Hardline.flat_alt(Self::Nil).into()
    }
}

impl DocumentTree
{
    /// The given text, which must not contain line breaks.
    #[inline]
    pub fn text<U: Into<Cow<'static, str>>>(data: U) -> Self {
        match data.into() {
            Cow::Borrowed(s) => DocumentTree::StaticText(s),
            Cow::Owned(s) => DocumentTree::Text(Rc::new(s)),
        }
    }

    fn flat_alt<D>(self, doc: D) -> Self
    {
        todo!()
        // DocBuilder(T::ALLOCATOR, self.into())
        //     .flat_alt(doc)
        //     .into_plain_doc()
    }
}

impl<'a, T, A, S> From<S> for DocumentTree
    where
        S: Into<Cow<'static, str>>,
{
    fn from(s: S) -> DocumentTree {
        DocumentTree::text(s)
    }
}

pub struct PrettyFmt<'a>
{
    doc: &'a DocumentTree,
    width: usize,
}

impl<'a> Display for PrettyFmt<'a>
{
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        self.doc.render_fmt(self.width, f)
    }
}

impl DocumentTree
{
    /// Writes a rendered document to a `std::io::Write` object.
    #[inline]
    pub fn render<W>(&self, width: usize, out: &mut W) -> std::io::Result<()>
        where
            W: ?Sized + std::io::Write,
    {
        self.render_raw(width, &mut IoWrite::new(out))
    }

    /// Writes a rendered document to a `std::fmt::Write` object.
    #[inline]
    pub fn render_fmt<W>(&self, width: usize, out: &mut W) -> core::fmt::Result
        where
            W: ?Sized + core::fmt::Write,
    {
        self.render_raw(width, &mut FmtWrite::new(out))
    }

    /// Writes a rendered document to a `RenderAnnotated<A>` object.
    #[inline]
    pub fn render_raw<W>(&self, width: usize, out: &mut W) -> Result<(), W::Error>
        where
                for<'b> W: render::RenderAnnotated<'b>,
                W: ?Sized,
    {
        render::best(self, width, out)
    }

    /// Returns a value which implements `std::fmt::Display`
    ///
    /// ```
    /// use pretty::{Doc, BoxDoc};
    /// let doc = BoxDoc::<()>::group(
    ///     BoxDoc::text("hello").append(Doc::line()).append(Doc::text("world"))
    /// );
    /// assert_eq!(format!("{}", doc.pretty(80)), "hello world");
    /// ```
    #[inline]
    pub fn pretty<'d>(&'d self, width: usize) -> PrettyFmt<'d> {
        PrettyFmt { doc: self, width }
    }
}

impl DocumentTree

{
    #[inline]
    pub fn render_colored<W>(&self, width: usize, out: W) -> std::io::Result<()>
        where
            W: WriteColor,
    {
        render::best(self, width, &mut TermColored::new(out))
    }
}

impl Add<Self> for DocumentTree {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::Append {
            base: Rc::new(self),
            rest: Rc::new(rhs),
        }
    }
}


/// Trait for types which can be converted to a `Document`
pub trait Pretty<'a, D>
    where
        D: ?Sized + DocAllocator<'a, A>,
{
    /// Converts `self` into a document
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A>;
}

impl<'a, A> Pretty<'a, RcAllocator, A> for RcDoc<'a, A>
    where
        A: 'a,
{
    fn pretty(self, allocator: &'a RcAllocator) -> DocBuilder<'a, RcAllocator, A> {
        DocBuilder(allocator, self.into())
    }
}

impl<'a, A> Pretty<'a, Arena<'a, A>, A> for RefDoc<'a, A>
    where
        A: 'a,
{
    fn pretty(self, allocator: &'a Arena<'a, A>) -> DocBuilder<'a, Arena<'a, A>, A> {
        DocBuilder(allocator, self.into())
    }
}

impl<'a, D> Pretty<'a, D, A> for BuildDoc<'a, D::Doc, A>
    where
        A: 'a,
        D: ?Sized + DocAllocator<'a, A>,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        DocBuilder(allocator, self)
    }
}

impl<'a, D> Pretty<'a, D, A> for DocumentTree<'a, D::Doc>
    where
        A: 'a,
        D: ?Sized + DocAllocator<'a, A>,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        DocBuilder(allocator, self.into())
    }
}

impl<'a, D> Pretty<'a, D, A> for DocBuilder<'a, D, A>
    where
        A: 'a,
        D: ?Sized + DocAllocator<'a, A>,
{
    fn pretty(self, _: &'a D) -> DocBuilder<'a, D, A> {
        self
    }
}

impl<'a, D, A, T> Pretty<'a, D, A> for Option<T>
    where
        A: 'a,
        D: ?Sized + DocAllocator<'a, A>,
        T: Pretty<'a, D, A>,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        match self {
            Some(x) => x.pretty(allocator),
            None => allocator.nil(),
        }
    }
}

impl<'a, D> Pretty<'a, D, A> for &'a str
    where
        A: 'a,
        D: ?Sized + DocAllocator<'a, A>,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        todo!()
        // allocator.text(self)
    }
}

impl<'a, D> Pretty<'a, D> for &'a String
    where

        D: ?Sized + DocAllocator<'a>,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        self[..].pretty(allocator)
    }
}

impl<'a, D> Pretty<'a, D> for String
    where

        D: ?Sized + DocAllocator<'a>,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        allocator.text(self)
    }
}


/// Concatenates a number of documents (or values that can be converted into a document via the
/// `Pretty` trait, like `&str`)
///
/// ```
/// use pretty::{docs, Arena, DocAllocator};
/// let arena = &Arena::<()>::new();
/// let doc = docs![
///     arena,
///     "let",
///     arena.softline(),
///     "x",
///     arena.softline(),
///     "=",
///     arena.softline(),
///     Some("123"),
/// ];
/// assert_eq!(doc.1.pretty(80).to_string(), "let x = 123");
/// ```
#[macro_export]
macro_rules! docs {
    ($alloc: expr, $first: expr $(,)?) => {
        $crate::Pretty::pretty($first, $alloc)
    };
    ($alloc: expr, $first: expr $(, $rest: expr)+ $(,)?) => {{
        let mut doc = $crate::Pretty::pretty($first, $alloc);
        $(
            doc = doc.append($rest);
        )*
        doc
    }}
}

impl<'a, D> DocBuilder<'a, D, A>
    where
        A: 'a,
        D: ?Sized + DocAllocator<'a, A>,
{
    fn with_utf8_len(self) -> Self {
        let s = match &*self {
            DocumentTree::Text(s) => &s[..],
            DocumentTree::StaticText(s) => s,
            // Doc::SmallText(s) => s,
            _ => return self,
        };
        use unicode_segmentation::UnicodeSegmentation;

        if s.is_ascii() {
            self
        } else {
            let grapheme_len = s.graphemes(true).count();
            let DocBuilder(allocator, _) = self;
            DocBuilder(
                allocator,
                DocumentTree::RenderLen(grapheme_len, self.into_doc()).into(),
            )
        }
    }

    /// Append the given document after this document.
    #[inline]
    pub fn append<E>(self, that: E) -> DocBuilder<'a, D, A>
        where
            E: Pretty<'a, D, A>,
    {
        let DocBuilder(allocator, _) = self;
        let that = that.pretty(allocator);
        match (&*self, &*that) {
            (DocumentTree::Nil, _) => that,
            (_, DocumentTree::Nil) => self,
            _ => DocBuilder(
                allocator,
                DocumentTree::Append(
                    allocator.alloc_cow(self.into()),
                    allocator.alloc_cow(that.into()),
                )
                    .into(),
            ),
        }
    }

    /// Acts as `self` when laid out on multiple lines and acts as `that` when laid out on a single line.
    ///
    /// ```
    /// use pretty::{Arena, DocAllocator};
    ///
    /// let arena = Arena::<()>::new();
    /// let body = arena.line().append("x");
    /// let doc = arena.text("let")
    ///     .append(arena.line())
    ///     .append("x")
    ///     .group()
    ///     .append(
    ///         body.clone()
    ///             .flat_alt(
    ///                 arena.line()
    ///                     .append("in")
    ///                     .append(body)
    ///             )
    ///     )
    ///     .group();
    ///
    /// assert_eq!(doc.1.pretty(100).to_string(), "let x in x");
    /// assert_eq!(doc.1.pretty(8).to_string(), "let x\nx");
    /// ```
    #[inline]
    pub fn flat_alt<E>(self, that: E) -> DocBuilder<'a, D, A>
        where
            E: Pretty<'a, D, A>,
    {
        let DocBuilder(allocator, this) = self;
        let that = that.pretty(allocator);
        DocBuilder(
            allocator,
            DocumentTree::FlatAlt(allocator.alloc_cow(this), allocator.alloc_cow(that.into())).into(),
        )
    }

    /// Mark this document as a group.
    ///
    /// Groups are layed out on a single line if possible.  Within a group, all basic documents with
    /// several possible layouts are assigned the same layout, that is, they are all layed out
    /// horizontally and combined into a one single line, or they are each layed out on their own
    /// line.
    #[inline]
    pub fn group(self) -> DocBuilder<'a, D, A> {
        match *self.1 {
            DocumentTree::Group(_)
            | DocumentTree::Text(_)
            | DocumentTree::StaticText(_)
            // | Doc::SmallText(_)
            | DocumentTree::Nil => self,
            _ => {
                let DocBuilder(allocator, this) = self;
                DocBuilder(allocator, DocumentTree::Group(allocator.alloc_cow(this)).into())
            }
        }
    }

    /// Increase the indentation level of this document.
    #[inline]
    pub fn nest(self, offset: isize) -> DocBuilder<'a, D, A> {
        if let DocumentTree::Nil = &*self.1 {
            return self;
        }
        if offset == 0 {
            return self;
        }
        let DocBuilder(allocator, this) = self;
        DocBuilder(
            allocator,
            DocumentTree::Nest(offset, allocator.alloc_cow(this)).into(),
        )
    }

    #[inline]
    pub fn annotate(self, ann: A) -> DocBuilder<'a, D, A> {
        let DocBuilder(allocator, this) = self;
        DocBuilder(
            allocator,
            DocumentTree::Annotated(ann, allocator.alloc_cow(this)).into(),
        )
    }

    #[inline]
    pub fn union<E>(self, other: E) -> DocBuilder<'a, D, A>
        where
            E: Into<BuildDoc<'a, D::Doc, A>>,
    {
        let DocBuilder(allocator, this) = self;
        let other = other.into();
        let doc = DocumentTree::Union(allocator.alloc_cow(this), allocator.alloc_cow(other));
        DocBuilder(allocator, doc.into())
    }

    /// Lays out `self` so with the nesting level set to the current column
    ///
    /// NOTE: The doc pointer type, `D` may need to be cloned. Consider using cheaply cloneable ptr
    /// like `RefDoc` or `RcDoc`
    ///
    /// ```rust
    /// use pretty::{docs, DocAllocator};
    ///
    /// let arena = &pretty::Arena::<()>::new();
    /// let doc = docs![
    ///     arena,
    ///     "lorem",
    ///     " ",
    ///     arena.intersperse(["ipsum", "dolor"].iter().cloned(), arena.line_()).align(),
    ///     arena.hardline(),
    ///     "next",
    /// ];
    /// assert_eq!(doc.1.pretty(80).to_string(), "lorem ipsum\n      dolor\nnext");
    /// ```
    #[inline]
    pub fn align(self) -> DocBuilder<'a, D, A>
        where
            DocBuilder<'a, D, A>: Clone,
    {
        let allocator = self.0;
        allocator.column(move |col| {
            let self_ = self.clone();
            allocator
                .nesting(move |nest| self_.clone().nest(col as isize - nest as isize).into_doc())
                .into_doc()
        })
    }

    /// Lays out `self` with a nesting level set to the current level plus `adjust`.
    ///
    /// NOTE: The doc pointer type, `D` may need to be cloned. Consider using cheaply cloneable ptr
    /// like `RefDoc` or `RcDoc`
    ///
    /// ```rust
    /// use pretty::DocAllocator;
    ///
    /// let arena = pretty::Arena::<()>::new();
    /// let doc = arena.text("prefix").append(arena.text(" "))
    ///     .append(arena.reflow("Indenting these words with nest").hang(4));
    /// assert_eq!(
    ///     doc.1.pretty(24).to_string(),
    ///     "prefix Indenting these\n           words with\n           nest",
    /// );
    /// ```
    #[inline]
    pub fn hang(self, adjust: isize) -> DocBuilder<'a, D, A>
        where
            DocBuilder<'a, D, A>: Clone,
    {
        self.nest(adjust).align()
    }

    /// Indents `self` by `adjust` spaces from the current cursor position
    ///
    /// NOTE: The doc pointer type, `D` may need to be cloned. Consider using cheaply cloneable ptr
    /// like `RefDoc` or `RcDoc`
    ///
    /// ```rust
    /// use pretty::DocAllocator;
    ///
    /// let arena = pretty::Arena::<()>::new();
    /// let doc = arena.text("prefix").append(arena.text(" "))
    ///     .append(arena.reflow("The indent function indents these words!").indent(4));
    /// assert_eq!(
    ///     doc.1.pretty(24).to_string(),
    /// "
    /// prefix     The indent
    ///            function
    ///            indents these
    ///            words!".trim_start(),
    /// );
    /// ```
    #[inline]
    pub fn indent(self, adjust: usize) -> DocBuilder<'a, D, A>
        where
            DocBuilder<'a, D, A>: Clone,
    {
        let spaces = {
            use crate::render::SPACES;
            let DocBuilder(allocator, _) = self;
            let mut doc = allocator.nil();
            let mut remaining = adjust;
            while remaining != 0 {
                let i = SPACES.len().min(remaining);
                remaining -= i;
                doc = doc.append(allocator.text(&SPACES[..i]))
            }
            doc
        };
        spaces.append(self).hang(adjust.try_into().unwrap())
    }

    /// Lays out `self` and provides the column width of it available to `f`
    ///
    /// NOTE: The doc pointer type, `D` may need to be cloned. Consider using cheaply cloneable ptr
    /// like `RefDoc` or `RcDoc`
    ///
    /// ```rust
    /// use pretty::DocAllocator;
    ///
    /// let arena = pretty::Arena::<()>::new();
    /// let doc = arena.text("prefix ")
    ///     .append(arena.column(|l| {
    ///         arena.text("| <- column ").append(arena.as_string(l)).into_doc()
    ///     }));
    /// assert_eq!(doc.1.pretty(80).to_string(), "prefix | <- column 7");
    /// ```
    #[inline]
    pub fn width(self, f: impl Fn(isize) -> D::Doc + 'a) -> DocBuilder<'a, D, A>
        where
            BuildDoc<'a, D::Doc, A>: Clone,
    {
        let DocBuilder(allocator, this) = self;
        let f = allocator.alloc_width_fn(f);
        allocator.column(move |start| {
            let f = f.clone();

            DocBuilder(allocator, this.clone())
                .append(allocator.column(move |end| f(end as isize - start as isize)))
                .into_doc()
        })
    }

    /// Puts `self` between `before` and `after`
    #[inline]
    pub fn enclose<E, F>(self, before: E, after: F) -> DocBuilder<'a, D, A>
        where
            E: Pretty<'a, D, A>,
            F: Pretty<'a, D, A>,
    {
        let DocBuilder(allocator, _) = self;
        DocBuilder(allocator, before.pretty(allocator).1)
            .append(self)
            .append(after)
    }

    pub fn single_quotes(self) -> DocBuilder<'a, D, A> {
        self.enclose("'", "'")
    }

    pub fn double_quotes(self) -> DocBuilder<'a, D, A> {
        self.enclose("\"", "\"")
    }
    pub fn parens(self) -> DocBuilder<'a, D, A> {
        self.enclose("(", ")")
    }

    pub fn angles(self) -> DocBuilder<'a, D, A> {
        self.enclose("<", ">")
    }
    pub fn braces(self) -> DocBuilder<'a, D, A> {
        self.enclose("{", "}")
    }

    pub fn brackets(self) -> DocBuilder<'a, D, A> {
        self.enclose("[", "]")
    }

    pub fn into_doc(self) -> D::Doc {
        match self.1 {
            BuildDoc::DocPtr(d) => d,
            BuildDoc::Doc(d) => self.0.alloc(d),
        }
    }

    fn into_plain_doc(self) -> DocumentTree<'a, D::Doc> {
        match self.1 {
            BuildDoc::DocPtr(_) => unreachable!(),
            BuildDoc::Doc(d) => d,
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! chain {
        ($first: expr $(, $rest: expr)* $(,)?) => {{
            #[allow(unused_mut)]
            let mut doc = DocBuilder(&BoxAllocator, $first.into());
            $(
                doc = doc.append($rest);
            )*
            doc.into_doc()
        }}
    }

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn doc_size() {
        // Safeguard against accidentally growing Doc
        assert_eq!(8 * 3, std::mem::size_of::<DocumentTree<RefDoc>>());
    }

    macro_rules! test {
        ($size:expr, $actual:expr, $expected:expr) => {
            let mut s = String::new();
            $actual.render_fmt($size, &mut s).unwrap();
            difference::assert_diff!(&s, $expected, "\n", 0);
        };
        ($actual:expr, $expected:expr) => {
            test!(70, $actual, $expected)
        };
    }

    #[test]
    fn box_doc_inference() {
        let doc: BoxDoc<()> = BoxDoc::group(
            BoxDoc::text("test")
                .append(BoxDoc::line())
                .append(BoxDoc::text("test")),
        );

        test!(doc, "test test");
    }

    #[test]
    fn newline_in_text() {
        let doc: BoxDoc<()> = BoxDoc::group(
            BoxDoc::text("test").append(
                BoxDoc::line()
                    .append(BoxDoc::text("\"test\n     test\""))
                    .nest(4),
            ),
        );

        test!(5, doc, "test\n    \"test\n     test\"");
    }

    #[test]
    fn forced_newline() {
        let doc: BoxDoc<()> = BoxDoc::group(
            BoxDoc::text("test")
                .append(BoxDoc::hardline())
                .append(BoxDoc::text("test")),
        );

        test!(doc, "test\ntest");
    }

    #[test]
    fn space_do_not_reset_pos() {
        let doc: BoxDoc<()> = BoxDoc::group(BoxDoc::text("test").append(BoxDoc::line()))
            .append(BoxDoc::text("test"))
            .append(BoxDoc::group(BoxDoc::line()).append(BoxDoc::text("test")));

        test!(9, doc, "test test\ntest");
    }

    // Tests that the `BoxDoc::hardline()` does not cause the rest of document to think that it fits on
    // a single line but instead breaks on the `BoxDoc::line()` to fit with 6 columns
    #[test]
    fn newline_does_not_cause_next_line_to_be_to_long() {
        let doc: RcDoc<()> = RcDoc::group(
            RcDoc::text("test").append(RcDoc::hardline()).append(
                RcDoc::text("test")
                    .append(RcDoc::line())
                    .append(RcDoc::text("test")),
            ),
        );

        test!(6, doc, "test\ntest\ntest");
    }

    #[test]
    fn newline_after_group_does_not_affect_it() {
        let arena = Arena::<()>::new();
        let doc = arena.text("x").append(arena.line()).append("y").group();

        test!(100, doc.append(arena.hardline()).1, "x y\n");
    }

    #[test]
    fn block() {
        let doc: RcDoc<()> = RcDoc::group(
            RcDoc::text("{")
                .append(
                    RcDoc::line()
                        .append(RcDoc::text("test"))
                        .append(RcDoc::line())
                        .append(RcDoc::text("test"))
                        .nest(2),
                )
                .append(RcDoc::line())
                .append(RcDoc::text("}")),
        );

        test!(5, doc, "{\n  test\n  test\n}");
    }

    #[test]
    fn block_with_hardline() {
        let doc: RcDoc<()> = RcDoc::group(
            RcDoc::text("{")
                .append(
                    RcDoc::line()
                        .append(RcDoc::text("test"))
                        .append(RcDoc::hardline())
                        .append(RcDoc::text("test"))
                        .nest(2),
                )
                .append(RcDoc::line())
                .append(RcDoc::text("}")),
        );

        test!(10, doc, "{\n  test\n  test\n}");
    }

    #[test]
    fn block_with_hardline_negative_nest() {
        let doc: RcDoc<()> = RcDoc::group(
            RcDoc::text("{")
                .append(
                    RcDoc::line()
                        .append(RcDoc::text("test"))
                        .append(RcDoc::hardline())
                        .append(RcDoc::text("test"))
                        .nest(-2),
                )
                .append(RcDoc::line())
                .append(RcDoc::text("}")),
        );

        test!(10, doc, "{\ntest\ntest\n}");
    }

    #[test]
    fn line_comment() {
        let doc: BoxDoc<()> = BoxDoc::group(
            BoxDoc::text("{")
                .append(
                    BoxDoc::line()
                        .append(BoxDoc::text("test"))
                        .append(BoxDoc::line())
                        .append(BoxDoc::text("// a").append(BoxDoc::hardline()))
                        .append(BoxDoc::text("test"))
                        .nest(2),
                )
                .append(BoxDoc::line())
                .append(BoxDoc::text("}")),
        );

        test!(14, doc, "{\n  test\n  // a\n  test\n}");
    }

    #[test]
    fn annotation_no_panic() {
        let doc: BoxDoc<()> = BoxDoc::group(
            BoxDoc::text("test")
                .annotate(())
                .append(BoxDoc::hardline())
                .annotate(())
                .append(BoxDoc::text("test")),
        );

        test!(doc, "test\ntest");
    }

    fn nest_on_line(doc: BoxDoc<'static, ()>) -> BoxDoc<'static, ()> {
        BoxDoc::softline().append(BoxDoc::nesting(move |n| {
            let doc = doc.clone();
            BoxDoc::column(move |c| {
                if n == c {
                    BoxDoc::text("  ").append(doc.clone()).nest(2)
                } else {
                    doc.clone()
                }
            })
        }))
    }

    #[test]
    fn hang_lambda1() {
        let doc = chain![
            chain!["let", BoxDoc::line(), "x", BoxDoc::line(), "="].group(),
            nest_on_line(chain![
                "\\y ->",
                chain![BoxDoc::line(), "y"].nest(2).group()
            ]),
        ]
            .group();

        test!(doc, "let x = \\y -> y");
        test!(
            8,
            doc,
            r"let x =
  \y ->
    y"
        );
        test!(
            14,
            doc,
            r"let x = \y ->
  y"
        );
    }

    #[test]
    fn hang_comment() {
        let body = chain!["y"].nest(2).group();
        let doc = chain![
            chain!["let", BoxDoc::line(), "x", BoxDoc::line(), "="].group(),
            nest_on_line(chain![
                "\\y ->",
                nest_on_line(chain!["// abc", BoxDoc::hardline(), body])
            ]),
        ]
            .group();

        test!(8, doc, "let x =\n  \\y ->\n    // abc\n    y");
        test!(14, doc, "let x = \\y ->\n  // abc\n  y");
    }

    #[test]
    fn union() {
        let doc = chain![
            chain!["let", BoxDoc::line(), "x", BoxDoc::line(), "="].group(),
            nest_on_line(chain![
                "(",
                chain![
                    BoxDoc::line_(),
                    chain!["x", ","].group(),
                    BoxDoc::line(),
                    chain!["1234567890", ","].group()
                ]
                .nest(2)
                .group(),
                BoxDoc::line_().append(")"),
            ])
        ]
            .group();

        test!(doc, "let x = (x, 1234567890,)");
        test!(8, doc, "let x =\n  (\n    x,\n    1234567890,\n  )");
        test!(14, doc, "let x = (\n  x,\n  1234567890,\n)");
    }

    fn hang2(
        from: BoxDoc<'static, ()>,
        body_whitespace: BoxDoc<'static, ()>,
        body: BoxDoc<'static, ()>,
        trailer: BoxDoc<'static, ()>,
    ) -> BoxDoc<'static, ()> {
        let body1 = body_whitespace
            .append(body.clone())
            .nest(2)
            .group()
            .append(trailer.clone());
        let body2 = BoxDoc::hardline()
            .append(body.clone())
            .nest(2)
            .group()
            .append(trailer.clone());

        let single = from.clone().append(body1.clone()).group();

        let hang = from.clone().append(body2).group();

        let break_all = from.append(body1).group().nest(2);

        BoxDoc::group(single.union(hang.union(break_all)))
    }

    #[test]
    fn hang_lambda2() {
        let from = chain![
            chain!["let", BoxDoc::line(), "x", BoxDoc::line(), "="].group(),
            BoxDoc::line(),
            "\\y ->",
        ]
            .group();

        let body = chain!["y"].group();

        let trailer = BoxDoc::nil();

        let doc = hang2(from, BoxDoc::line(), body, trailer);
        eprintln!("{:#?}", doc);

        test!(doc, "let x = \\y -> y");
        test!(14, doc, "let x = \\y ->\n  y");
    }

    #[test]
    fn union2() {
        let from = chain![
            chain!["let", BoxDoc::line(), "x", BoxDoc::line(), "="].group(),
            BoxDoc::line(),
            "(",
        ]
            .group();

        let body = chain![
            chain!["x", ","].group(),
            BoxDoc::line(),
            chain!["1234567890", ","].group()
        ]
            .group();

        let trailer = BoxDoc::line_().append(")");

        let doc = hang2(from, BoxDoc::line_(), body, trailer);

        test!(doc, "let x = (x, 1234567890,)");
        test!(14, doc, "let x = (\n  x,\n  1234567890,\n)");
    }

    #[test]
    fn usize_max_value() {
        let doc: BoxDoc<()> = BoxDoc::group(
            BoxDoc::text("test")
                .append(BoxDoc::line())
                .append(BoxDoc::text("test")),
        );

        test!(usize::max_value(), doc, "test test");
    }

    #[test]
    fn fail() {
        let fail_break: BoxDoc<()> = BoxDoc::fail().flat_alt(DocumentTree::nil());

        let doc = fail_break.append(DocumentTree::text("12345")).group().union("abc");

        test!(5, doc, "12345");
        test!(4, doc, "abc");
    }

    pub struct TestWriter<W> {
        upstream: W,
    }

    impl<W> TestWriter<W> {
        pub fn new(upstream: W) -> Self {
            Self { upstream }
        }
    }

    impl<W> Render for TestWriter<W>
        where
            W: Render,
    {
        type Error = W::Error;

        fn write_str(&mut self, s: &str) -> Result<usize, W::Error> {
            self.upstream.write_str(s)
        }

        fn write_str_all(&mut self, s: &str) -> Result<(), W::Error> {
            self.upstream.write_str_all(s)
        }

        fn fail_doc(&self) -> Self::Error {
            self.upstream.fail_doc()
        }
    }

    impl<W> RenderAnnotated<'_, ()> for TestWriter<W>
        where
            W: Render,
    {
        fn push_annotation(&mut self, _: &()) -> Result<(), Self::Error> {
            self.upstream.write_str_all("[")
        }

        fn pop_annotation(&mut self) -> Result<(), Self::Error> {
            self.upstream.write_str_all("]")
        }
    }

    #[test]
    fn annotations() {
        let actual = BoxDoc::text("abc").annotate(()).annotate(());
        let mut s = String::new();
        actual
            .render_raw(70, &mut TestWriter::new(FmtWrite::new(&mut s)))
            .unwrap();
        difference::assert_diff!(&s, "[[abc]]", "\n", 0);
    }

    #[test]
    fn non_ascii_is_not_byte_length() {
        let doc: BoxDoc<()> = BoxDoc::group(
            BoxDoc::text("ÅÄÖ")
                .append(BoxDoc::line())
                .append(BoxDoc::text("test")),
        );

        test!(8, doc, "ÅÄÖ test");
    }
}
