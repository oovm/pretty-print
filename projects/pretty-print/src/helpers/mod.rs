#![doc = include_str!("readme.md")]

use crate::{DocumentTree, PrettyBuilder, PrettyPrint, PrettyProvider};
use alloc::vec::Vec;
use core::ops::AddAssign;

mod k_and_r_bracket;
mod sequence;
// mod affixes;

pub use self::{k_and_r_bracket::KAndRBracket, sequence::DocumentSequence};
