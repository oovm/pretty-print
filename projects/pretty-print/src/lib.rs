// #![cfg_attr(not(feature = "std"), no_std)]
// #![deny(missing_debug_implementations, missing_copy_implementations)]
// #![warn(missing_docs, rustdoc::missing_crate_level_docs)]
#![doc = include_str!("../readme.md")]
#![doc(html_logo_url = "https://raw.githubusercontent.com/oovm/shape-rs/dev/projects/images/Trapezohedron.svg")]
#![doc(html_favicon_url = "https://raw.githubusercontent.com/oovm/shape-rs/dev/projects/images/Trapezohedron.svg")]

extern crate alloc;


mod blocks;
mod providers;
mod traits;

pub use crate::{blocks::k_and_r_bracket::KAndRBracket, providers::PrettyProvider, traits::PrettyPrint};
use core::fmt::{Debug, Formatter};

use std::{borrow::Cow, convert::TryInto, fmt::Display, ops::Add, rc::Rc};
use std::ops::AddAssign;

use termcolor::{ColorSpec, WriteColor};

// pub mod block;
mod render;
mod tree;

pub use crate::tree::{DocumentTree, DocumentSequence};

// pub use self::block::{Affixes, BlockDoc};

pub use self::render::{FmtWrite, IoWrite, Render, RenderAnnotated, terminal::TerminalWriter};

/// The given text, which must not contain line breaks.
pub struct PrettyFormatter<'a> {
    tree: &'a DocumentTree,
    width: usize,
}

impl<'a> Display for PrettyFormatter<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        self.tree.render_fmt(self.width, f)
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

use unicode_segmentation::UnicodeSegmentation;
