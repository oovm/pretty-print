use crate::{render, FmtWrite, PrettyBuilder, PrettyPrint, PrettyProvider, RenderAnnotated};
use alloc::{borrow::Cow, rc::Rc, string::String};
use color_ansi::AnsiStyle;
use core::{
    fmt::{Debug, Formatter},
    ops::{Add, AddAssign},
};
use std::io::Write;
use unicode_segmentation::UnicodeSegmentation;

mod into;

/// The concrete document type. This type is not meant to be used directly. Instead use the static
/// functions on `Doc` or the methods on an `DocAllocator`.
///
/// The `T` parameter is used to abstract over pointers to `Doc`. See `RefDoc` and `BoxDoc` for how
/// it is used
pub enum PrettyTree {
    /// Nothing to show
    Nil,
    /// A hard line break
    Hardline,
    Text(Rc<str>),
    /// Concatenates two documents
    StaticText(&'static str),
    Annotated {
        color: Rc<AnsiStyle>,
        doc: Rc<Self>,
    },
    /// Concatenates two documents
    Append {
        lhs: Rc<Self>,
        rhs: Rc<Self>,
    },
    /// Concatenates two documents with a space in between
    Group {
        items: Rc<Self>,
    },
    /// Concatenates two documents with a line in between
    MaybeInline {
        block: Rc<Self>,
        inline: Rc<Self>,
    },
    /// Concatenates two documents with a line in between
    Nest {
        space: isize,
        doc: Rc<Self>,
    },
    // Stores the length of a string document that is not just ascii
    RenderLen {
        len: usize,
        doc: Rc<Self>,
    },
    Union {
        left: Rc<Self>,
        right: Rc<Self>,
    },
    Column {
        function: Rc<dyn Fn(usize) -> Self>,
    },
    Nesting {
        function: Rc<dyn Fn(usize) -> Self>,
    },
    /// Concatenates two documents with a line in between
    Fail,
}

impl Clone for PrettyTree {
    fn clone(&self) -> Self {
        match self {
            Self::Nil => Self::Nil,
            Self::Hardline => Self::Hardline,
            Self::Text(s) => Self::Text(s.clone()),
            Self::StaticText(s) => Self::StaticText(*s),
            Self::Annotated { color, doc } => Self::Annotated { color: color.clone(), doc: doc.clone() },
            Self::Append { lhs, rhs } => Self::Append { lhs: lhs.clone(), rhs: rhs.clone() },
            Self::Group { items } => Self::Group { items: items.clone() },
            Self::MaybeInline { block, inline } => Self::MaybeInline { block: block.clone(), inline: inline.clone() },
            Self::Nest { space, doc } => Self::Nest { space: *space, doc: doc.clone() },
            Self::RenderLen { len, doc } => Self::RenderLen { len: *len, doc: doc.clone() },
            Self::Union { left, right } => Self::Union { left: left.clone(), right: right.clone() },
            Self::Column { function: column } => Self::Column { function: column.clone() },
            Self::Nesting { function: nesting } => Self::Nesting { function: nesting.clone() },
            Self::Fail => Self::Fail,
        }
    }
}

impl Default for PrettyTree {
    fn default() -> Self {
        Self::Nil
    }
}

fn append_docs(mut doc: &PrettyTree, consumer: &mut impl FnMut(&PrettyTree)) {
    loop {
        match doc {
            PrettyTree::Append { lhs, rhs } => {
                append_docs(lhs, consumer);
                doc = rhs;
            }
            _ => break consumer(doc),
        }
    }
}

impl Debug for PrettyTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        let is_line = |doc: &PrettyTree| match doc {
            PrettyTree::MaybeInline { block: flat, inline: alt } => {
                matches!((&**flat, &**alt), (PrettyTree::Hardline, PrettyTree::StaticText(" ")))
            }
            _ => false,
        };
        let is_line_ = |doc: &PrettyTree| match doc {
            PrettyTree::MaybeInline { block: flat, inline: alt } => {
                matches!((&**flat, &**alt), (PrettyTree::Hardline, PrettyTree::Nil))
            }
            _ => false,
        };
        match self {
            PrettyTree::Nil => f.debug_tuple("Nil").finish(),
            PrettyTree::Append { lhs: base, rhs: rest } => {
                let mut f = f.debug_list();
                append_docs(self, &mut |doc| {
                    f.entry(doc);
                });
                f.finish()
            }
            _ if is_line(self) => f.debug_tuple("Line").finish(),
            _ if is_line_(self) => f.debug_tuple("Line_").finish(),
            PrettyTree::MaybeInline { block, inline } => f.debug_tuple("FlatAlt").field(block).field(inline).finish(),
            PrettyTree::Group { items } => {
                if is_line(self) {
                    return f.debug_tuple("SoftLine").finish();
                }
                if is_line_(self) {
                    return f.debug_tuple("SoftLine?").finish();
                }
                f.debug_tuple("Group").field(items).finish()
            }
            PrettyTree::Nest { space, doc } => f.debug_tuple("Nest").field(&space).field(doc).finish(),
            PrettyTree::Hardline => f.debug_tuple("Hardline").finish(),
            PrettyTree::RenderLen { doc, .. } => doc.fmt(f),
            PrettyTree::Text(s) => Debug::fmt(s, f),
            PrettyTree::StaticText(s) => Debug::fmt(s, f),
            PrettyTree::Annotated { color, doc } => f.debug_tuple("Annotated").field(color).field(doc).finish(),
            PrettyTree::Union { left, right } => f.debug_tuple("Union").field(left).field(right).finish(),
            PrettyTree::Column { .. } => f.debug_tuple("Column(..)").finish(),
            PrettyTree::Nesting { .. } => f.debug_tuple("Nesting(..)").finish(),
            PrettyTree::Fail => f.debug_tuple("Fail").finish(),
        }
    }
}

#[allow(non_upper_case_globals)]
impl PrettyTree {
    pub const Space: Self = PrettyTree::StaticText(" ");
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

impl PrettyTree {
    /// The given text, which must not contain line breaks.
    #[inline]
    pub fn text<U: Into<Cow<'static, str>>>(data: U) -> Self {
        match data.into() {
            Cow::Borrowed(s) => PrettyTree::StaticText(s),
            Cow::Owned(s) => PrettyTree::Text(Rc::from(s)),
        }
        .with_utf8_len()
    }
}

impl PrettyTree {
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
        W: RenderAnnotated,
        W: ?Sized,
    {
        render::best(Rc::new(self.clone()), width, out)
    }
}

impl PrettyTree {
    #[inline]
    #[cfg(feature = "std")]
    pub fn render_colored<W: Write>(&self, width: usize, out: W) -> std::io::Result<()> {
        render::best(Rc::new(self.clone()), width, &mut crate::TerminalWriter::new(out))
    }
}

impl PrettyBuilder for PrettyTree {
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
        E: Into<PrettyTree>,
    {
        Self::MaybeInline { block: Rc::new(self), inline: Rc::new(flat.into()) }
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
    fn indent(self, adjust: usize) -> Self {
        let spaces = {
            use crate::render::SPACES;
            let mut doc = PrettyTree::Nil;
            let mut remaining = adjust;
            while remaining != 0 {
                let i = SPACES.len().min(remaining);
                remaining -= i;
                doc = doc.append(PrettyTree::text(&SPACES[..i]))
            }
            doc
        };
        spaces.append(self).hang(adjust.try_into().unwrap())
    }
}

impl PrettyPrint for PrettyTree {
    fn pretty(&self, _: &PrettyProvider) -> PrettyTree {
        self.clone()
    }
}

impl PrettyTree {
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
        E: Into<PrettyTree>,
    {
        let rhs = follow.into();
        match (&self, &rhs) {
            (Self::Nil, _) => rhs,
            (_, Self::Nil) => self,
            _ => Self::Append { lhs: Rc::new(self), rhs: Rc::new(rhs) },
        }
    }
    /// Allocate a document that intersperses the given separator `S` between the given documents
    /// `[A, B, C, ..., Z]`, yielding `[A, S, B, S, C, S, ..., S, Z]`.
    ///
    /// Compare [the `intersperse` method from the `itertools` crate](https://docs.rs/itertools/0.5.9/itertools/trait.Itertools.html#method.intersperse).
    ///
    /// NOTE: The separator type, `S` may need to be cloned. Consider using cheaply cloneable ptr
    /// like `RefDoc` or `RcDoc`
    #[inline]
    pub fn join<I, T>(terms: I, joint: T) -> PrettyTree
    where
        I: IntoIterator<Item = PrettyTree>,
        T: Into<PrettyTree>,
    {
        let joint = joint.into();
        let mut out = PrettyTree::Nil;
        for term in terms.into_iter() {
            out = out.append(joint.clone()).append(term);
        }
        out
    }
    pub fn concat<I>(docs: I) -> Self
    where
        I: IntoIterator,
        I::Item: Into<PrettyTree>,
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
    /// Mark this document as a comment.
    #[inline]
    pub fn annotate(self, style: Rc<AnsiStyle>) -> Self {
        Self::Annotated { color: style, doc: Rc::new(self) }
    }
    /// Mark this document as a hard line break.
    #[inline]
    pub fn union<E>(self, other: E) -> Self
    where
        E: Into<PrettyTree>,
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
        Self::Column {
            function: Rc::new(move |col| {
                let self_ = self.clone();
                Self::Nesting { function: Rc::new(move |nest| self_.clone().nest(col as isize - nest as isize)) }
            }),
        }
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

    /// Puts `self` between `before` and `after` if `cond` is true
    pub fn single_quotes(self) -> Self {
        self.enclose("'", "'")
    }

    /// Puts `self` between `before` and `after` if `cond` is true
    pub fn double_quotes(self) -> Self {
        self.enclose("\"", "\"")
    }
    /// Puts `self` between `before` and `after` if `cond` is true
    pub fn parens(self) -> Self {
        self.enclose("(", ")")
    }
    /// Puts `self` between `before` and `after` if `cond` is true
    pub fn angles(self) -> Self {
        self.enclose("<", ">")
    }
    /// Puts `self` between `before` and `after` if `cond` is true
    pub fn braces(self) -> Self {
        self.enclose("{", "}")
    }
    /// Puts `self` between `before` and `after` if `cond` is true
    pub fn brackets(self) -> Self {
        self.enclose("[", "]")
    }
}
