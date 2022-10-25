#![cfg_attr(not(feature = "std"), no_std)]
#![deny(missing_debug_implementations, missing_copy_implementations)]
#![warn(missing_docs, rustdoc::missing_crate_level_docs)]
#![doc = include_str!("../readme.md")]
#![doc(html_logo_url = "https://raw.githubusercontent.com/oovm/shape-rs/dev/projects/images/Trapezohedron.svg")]
#![doc(html_favicon_url = "https://raw.githubusercontent.com/oovm/shape-rs/dev/projects/images/Trapezohedron.svg")]

extern crate alloc;
extern crate core;

pub mod helpers;
mod providers;
mod render;
mod traits;
mod tree;

pub use self::render::{FmtWrite, PrettyFormatter, Render, RenderAnnotated};
#[cfg(feature = "std")]
pub use crate::render::{terminal::TerminalWriter, IoWrite};
pub use crate::{
    providers::PrettyProvider,
    traits::{PrettyBuilder, PrettyPrint},
    tree::DocumentTree,
};
pub use color_ansi::*;

/// Concatenates a number of documents (or values that can be converted into a document via the
/// `Pretty` trait, like `&str`)
///
/// ```
/// use pretty_print::docs;
/// let doc =
///     docs!["let", arena.softline(), "x", arena.softline(), "=", arena.softline(), Some("123"),];
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
