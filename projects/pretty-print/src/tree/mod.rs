use crate::{render, FmtWrite, PrettyBuilder, PrettyFormatter, RenderAnnotated};
use alloc::{borrow::Cow, rc::Rc, string::String};
use core::{
    fmt::{Debug, Formatter},
    ops::{Add, AddAssign},
};
use termcolor::{ColorSpec, WriteColor};
use unicode_segmentation::UnicodeSegmentation;

mod into;

/// The concrete document type. This type is not meant to be used directly. Instead use the static
/// functions on `Doc` or the methods on an `DocAllocator`.
///
/// The `T` parameter is used to abstract over pointers to `Doc`. See `RefDoc` and `BoxDoc` for how
/// it is used
#[derive(Clone)]
pub enum DocumentTree {
    Nil,
    Append { lhs: Rc<Self>, rhs: Rc<Self> },
    // Sequence {
    //     items: Vec<Self>,
    // },
    Group { items: Rc<Self> },
    FlatAlt { block: Rc<Self>, inline: Rc<Self> },
    Nest { space: isize, doc: Rc<Self> },
    Hardline,
    // Stores the length of a string document that is not just ascii
    RenderLen { len: usize, doc: Rc<Self> },
    Text(Rc<str>),
    StaticText(&'static str),
    Annotated { color: ColorSpec, doc: Rc<Self> },
    Union { left: Rc<Self>, right: Rc<Self> },
    Column { column: fn(usize) -> Self },
    Nesting { nesting: fn(usize) -> Self },
    Fail,
}

impl Default for DocumentTree {
    fn default() -> Self {
        Self::Nil
    }
}

fn append_docs(mut doc: &DocumentTree, consumer: &mut impl FnMut(&DocumentTree)) {
    loop {
        match doc {
            DocumentTree::Append { lhs, rhs } => {
                append_docs(lhs, consumer);
                doc = rhs;
            }
            _ => break consumer(doc),
        }
    }
}

impl Debug for DocumentTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        let is_line = |doc: &DocumentTree| match doc {
            DocumentTree::FlatAlt { block: flat, inline: alt } => {
                matches!((&**flat, &**alt), (DocumentTree::Hardline, DocumentTree::StaticText(" ")))
            }
            _ => false,
        };
        let is_line_ = |doc: &DocumentTree| match doc {
            DocumentTree::FlatAlt { block: flat, inline: alt } => {
                matches!((&**flat, &**alt), (DocumentTree::Hardline, DocumentTree::Nil))
            }
            _ => false,
        };
        match self {
            DocumentTree::Nil => f.debug_tuple("Nil").finish(),
            DocumentTree::Append { lhs: base, rhs: rest } => {
                let mut f = f.debug_list();
                append_docs(self, &mut |doc| {
                    f.entry(doc);
                });
                f.finish()
            }
            _ if is_line(self) => f.debug_tuple("Line").finish(),
            _ if is_line_(self) => f.debug_tuple("Line_").finish(),
            DocumentTree::FlatAlt { block, inline } => f.debug_tuple("FlatAlt").field(block).field(inline).finish(),
            DocumentTree::Group { items } => {
                if is_line(self) {
                    return f.debug_tuple("SoftLine").finish();
                }
                if is_line_(self) {
                    return f.debug_tuple("SoftLine?").finish();
                }
                f.debug_tuple("Group").field(items).finish()
            }
            DocumentTree::Nest { space, doc } => f.debug_tuple("Nest").field(&space).field(doc).finish(),
            DocumentTree::Hardline => f.debug_tuple("Hardline").finish(),
            DocumentTree::RenderLen { doc, .. } => doc.fmt(f),
            DocumentTree::Text(s) => Debug::fmt(s, f),
            DocumentTree::StaticText(s) => Debug::fmt(s, f),
            DocumentTree::Annotated { color, doc } => f.debug_tuple("Annotated").field(color).field(doc).finish(),
            DocumentTree::Union { left, right } => f.debug_tuple("Union").field(left).field(right).finish(),
            DocumentTree::Column { .. } => f.debug_tuple("Column(..)").finish(),
            DocumentTree::Nesting { .. } => f.debug_tuple("Nesting(..)").finish(),
            DocumentTree::Fail => f.debug_tuple("Fail").finish(),
        }
    }
}

#[allow(non_upper_case_globals)]
impl DocumentTree {
    pub const Space: Self = DocumentTree::StaticText(" ");
    ///   A line acts like a  `\n`  but behaves like  `space`  if it is grouped on a single line.
    #[inline]
    pub fn line() -> Self {
        Self::Hardline.flat_alt(Self::Space).into()
    }

    ///   Acts like  `line`  but behaves like  `nil`  if grouped on a single line
    #[inline]
    pub fn line_() -> Self {
        Self::Hardline.flat_alt(Self::Nil).into()
    }
}

impl DocumentTree {
    /// The given text, which must not contain line breaks.
    #[inline]
    pub fn text<U: Into<Cow<'static, str>>>(data: U) -> Self {
        match data.into() {
            Cow::Borrowed(s) => DocumentTree::StaticText(s),
            Cow::Owned(s) => DocumentTree::Text(Rc::from(s)),
        }
        .with_utf8_len()
    }
}

impl DocumentTree {
    /// Writes a rendered document to a `std::io::Write` object.
    #[inline]
    #[cfg(feature = "std")]
    pub fn render<W>(&self, width: usize, out: &mut W) -> std::io::Result<()>
    where
        W: ?Sized + std::io::Write,
    {
        self.render_raw(width, &mut crate::IoWrite::new(out))
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
        for<'b> W: RenderAnnotated,
        W: ?Sized,
    {
        render::best(self, width, out)
    }

    /// Returns a value which implements `std::fmt::Display`
    ///
    /// ```
    /// use pretty::{BoxDoc, Doc};
    /// let doc =
    ///     BoxDoc::<()>::group(BoxDoc::text("hello").append(Doc::line()).append(Doc::text("world")));
    /// assert_eq!(format!("{}", doc.pretty(80)), "hello world");
    /// ```
    #[inline]
    pub fn pretty(&self, width: usize) -> PrettyFormatter {
        PrettyFormatter { tree: self, width }
    }
}

impl DocumentTree {
    #[inline]
    #[cfg(feature = "std")]
    pub fn render_colored<W>(&self, width: usize, out: W) -> std::io::Result<()>
    where
        W: WriteColor,
    {
        render::best(self, width, &mut crate::TerminalWriter::new(out))
    }
}

impl PrettyBuilder for DocumentTree {
    /// Acts as `self` when laid out on multiple lines and acts as `that` when laid out on a single line.
    ///
    /// ```
    /// use pretty::{Arena, DocAllocator};
    ///
    /// let arena = Arena::<()>::new();
    /// let body = arena.line().append("x");
    /// let doc = arena
    ///     .text("let")
    ///     .append(arena.line())
    ///     .append("x")
    ///     .group()
    ///     .append(body.clone().flat_alt(arena.line().append("in").append(body)))
    ///     .group();
    ///
    /// assert_eq!(doc.1.pretty(100).to_string(), "let x in x");
    /// assert_eq!(doc.1.pretty(8).to_string(), "let x\nx");
    /// ```
    #[inline]
    fn flat_alt<E>(self, flat: E) -> Self
    where
        E: Into<DocumentTree>,
    {
        Self::FlatAlt { block: Rc::new(self), inline: Rc::new(flat.into()) }
    }
}

impl DocumentTree {
    fn with_utf8_len(self) -> Self {
        let s = match &self {
            Self::Text(s) => s.as_ref(),
            Self::StaticText(s) => s,
            // Doc::SmallText(s) => s,
            _ => return self,
        };
        if s.is_ascii() {
            self
        }
        else {
            let grapheme_len = s.graphemes(true).count();
            Self::RenderLen { len: grapheme_len, doc: Rc::new(self) }
        }
    }

    /// Append the given document after this document.
    #[inline]
    pub fn append<E>(self, follow: E) -> Self
    where
        E: Into<DocumentTree>,
    {
        let rhs = follow.into();
        match (&self, &rhs) {
            (Self::Nil, _) => rhs,
            (_, Self::Nil) => self,
            _ => Self::Append { lhs: Rc::new(self), rhs: Rc::new(rhs) },
        }
    }

    pub fn concat<I>(docs: I) -> Self
    where
        I: IntoIterator,
        I::Item: Into<DocumentTree>,
    {
        let mut head = Self::Nil;
        for item in docs.into_iter() {
            head += item.into();
        }
        head
    }

    /// Mark this document as a group.
    ///
    /// Groups are layed out on a single line if possible.  Within a group, all basic documents with
    /// several possible layouts are assigned the same layout, that is, they are all layed out
    /// horizontally and combined into a one single line, or they are each layed out on their own
    /// line.
    #[inline]
    pub fn group(self) -> Self {
        match self {
            Self::Group { .. } | Self::Text(_) | Self::StaticText(_) | Self::Nil => self,
            _ => Self::Group { items: Rc::new(self) },
        }
    }

    /// Increase the indentation level of this document.
    #[inline]
    pub fn nest(self, offset: isize) -> Self {
        match self {
            Self::Nil => {
                return self;
            }
            _ => {}
        }
        if offset == 0 {
            return self;
        }
        Self::Nest { space: offset, doc: Rc::new(self) }
    }

    #[inline]
    pub fn annotate(self, ann: ColorSpec) -> Self {
        Self::Annotated { color: ann, doc: Rc::new(self) }
    }

    #[inline]
    pub fn union<E>(self, other: E) -> Self
    where
        E: Into<DocumentTree>,
    {
        Self::Union { left: Rc::new(self), right: Rc::new(other.into()) }
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
    pub fn align(self) -> Self {
        todo!()
        // Self::Column {
        //     column: move |col| {
        //         let self_ = self.clone();
        //         Self::Nesting {
        //             nesting: move |nest| self_.clone().nest(col as isize - nest as isize),
        //         }
        //     },
        // }
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
    /// let doc = arena
    ///     .text("prefix")
    ///     .append(arena.text(" "))
    ///     .append(arena.reflow("Indenting these words with nest").hang(4));
    /// assert_eq!(
    ///     doc.1.pretty(24).to_string(),
    ///     "prefix Indenting these\n           words with\n           nest",
    /// );
    /// ```
    #[inline]
    pub fn hang(self, adjust: isize) -> Self {
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
    /// let doc = arena
    ///     .text("prefix")
    ///     .append(arena.text(" "))
    ///     .append(arena.reflow("The indent function indents these words!").indent(4));
    /// assert_eq!(
    ///     doc.1.pretty(24).to_string(),
    ///     "
    /// prefix     The indent
    ///            function
    ///            indents these
    ///            words!"
    ///         .trim_start(),
    /// );
    /// ```
    #[inline]
    pub fn indent(self, adjust: usize) -> Self {
        let spaces = {
            use crate::render::SPACES;
            let mut doc = DocumentTree::Nil;
            let mut remaining = adjust;
            while remaining != 0 {
                let i = SPACES.len().min(remaining);
                remaining -= i;
                doc = doc.append(DocumentTree::text(&SPACES[..i]))
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
    /// let doc = arena
    ///     .text("prefix ")
    ///     .append(arena.column(|l| arena.text("| <- column ").append(arena.as_string(l)).into_doc()));
    /// assert_eq!(doc.1.pretty(80).to_string(), "prefix | <- column 7");
    /// ```
    #[inline]
    pub fn width(self, f: impl Fn(isize) -> Self) -> Self {
        todo!()
        // let f = allocator.alloc_width_fn(f);
        // allocator.column(move |start| {
        //     let f = f.clone();
        //
        //     DocumentTree(allocator, this.clone())
        //         .append(allocator.column(move |end| f(end as isize - start as isize)))
        //         .into_doc()
        // })
    }

    /// Puts `self` between `before` and `after`
    #[inline]
    pub fn enclose<E, F>(self, before: E, after: F) -> Self
    where
        E: Into<Self>,
        F: Into<Self>,
    {
        before.into().append(self).append(after.into())
    }

    pub fn single_quotes(self) -> Self {
        self.enclose("'", "'")
    }

    pub fn double_quotes(self) -> Self {
        self.enclose("\"", "\"")
    }
    pub fn parens(self) -> Self {
        self.enclose("(", ")")
    }

    pub fn angles(self) -> Self {
        self.enclose("<", ">")
    }
    pub fn braces(self) -> Self {
        self.enclose("{", "}")
    }

    pub fn brackets(self) -> Self {
        self.enclose("[", "]")
    }
}
