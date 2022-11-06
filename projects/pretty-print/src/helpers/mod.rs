#![doc = include_str!("readme.md")]

use crate::{PrettyBuilder, PrettyPrint, PrettyProvider, PrettyTree};
use alloc::vec::Vec;
use core::ops::AddAssign;

mod hard_block;
mod k_and_r_bracket;
mod sequence;
mod soft_block;
// mod affixes;

pub use self::{hard_block::HardBlock, k_and_r_bracket::KAndRBracket, sequence::PrettySequence, soft_block::SoftBlock};
