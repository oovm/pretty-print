use crate::{providers::PrettyProvider, PrettyTree};
use alloc::string::String;

pub mod printer;

/// The `PrettyPrint` trait is implemented by types that can be pretty-printed.
pub trait PrettyBuilder {
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
    fn flat_alt<E>(self, inline: E) -> PrettyTree
    where
        E: Into<PrettyTree>;
    /// Acts as `self` when laid out on a single line and acts as `that` when laid out on multiple lines.
    fn indent(self, indent: usize) -> PrettyTree;

    /// Increase the indentation level of this document.
    fn nest(self, offset: isize) -> PrettyTree;
}
