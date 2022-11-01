use crate::{providers::PrettyProvider, PrettyTree};
use alloc::string::String;

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
    fn indent(self, indent: usize) -> PrettyTree;
}

/// Marker trait for types that can be pretty printed.
pub trait PrettyPrint {
    /// Build a pretty tree for this type.
    fn pretty(&self, theme: &PrettyProvider) -> PrettyTree;
    /// Get a pretty string for this type.
    fn pretty_string(&self, width: usize) -> String {
        let arena = PrettyProvider::new(144);
        let mut buffer = String::new();
        if let Err(e) = self.pretty(&arena).render_fmt(width, &mut buffer) {
            panic!("Error: {}", e);
        }
        buffer
    }
    /// Print a pretty string for this type.
    fn pretty_colorful(&self, theme: &PrettyProvider) -> String {
        let mut buffer = vec![];
        if let Err(e) = self.pretty(&theme).render_colored(theme.get_width(), &mut buffer) {
            panic!("Error: {}", e);
        }
        match String::from_utf8(buffer) {
            Ok(s) => s,
            Err(e) => panic!("Error: {}", e),
        }
    }
}

impl PrettyPrint for &'static str {
    fn pretty(&self, _: &PrettyProvider) -> PrettyTree {
        PrettyTree::StaticText(*self)
    }
}
