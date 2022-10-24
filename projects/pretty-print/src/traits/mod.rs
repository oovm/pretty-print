use crate::{providers::PrettyProvider, DocumentTree};
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
    fn flat_alt<E>(self, inline: E) -> DocumentTree
    where
        E: Into<DocumentTree>;
}

/// Marker trait for types that can be pretty printed.
pub trait PrettyPrint {
    /// Build a pretty tree for this type.
    fn pretty(&self, allocator: &PrettyProvider) -> DocumentTree;
    /// Get a pretty string for this type.
    fn pretty_string(&self, width: usize) -> String {
        let arena = PrettyProvider::new();
        let mut buffer = String::new();
        if let Err(e) = self.pretty(&arena).render_fmt(width, &mut buffer) {
            return alloc::format!("Error: {}", e);
        }
        buffer
    }
    /// Print a pretty string for this type.
    #[cfg(feature = "std")]
    fn pretty_print(&self, width: usize) {
        let arena = PrettyProvider::new();
        let mut buffer = Buffer::ansi();
        match self.pretty(&arena).render_colored(width, &mut buffer) {
            Ok(_) => {
                println!("{}", unsafe { String::from_utf8_unchecked(buffer.into_inner()) });
            }
            Err(e) => {
                eprintln!("Error: {}", e);
            }
        }
    }
}
