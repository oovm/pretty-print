#![cfg_attr(not(feature = "std"), no_std)]
#![deny(missing_debug_implementations, missing_copy_implementations)]
#![warn(missing_docs, rustdoc::missing_crate_level_docs)]
#![doc = include_str!("../readme.md")]
#![doc(html_logo_url = "https://raw.githubusercontent.com/oovm/shape-rs/dev/projects/images/Trapezohedron.svg")]
#![doc(html_favicon_url = "https://raw.githubusercontent.com/oovm/shape-rs/dev/projects/images/Trapezohedron.svg")]

extern crate alloc;

mod blocks;
mod providers;
mod traits;

pub use crate::{
    providers::{PrettyProvider, PrettyTree},
    traits::PrettyPrint,
    blocks::k_and_r_bracket::KAndRBracket,
};
