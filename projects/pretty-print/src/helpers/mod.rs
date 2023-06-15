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

/// Represents a pretty-printable tree provider.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PrettyPrintKind {
    /// A normal text.
    Normal,
    /// A keyword.
    Keyword,
    /// A string.
    String,
    /// A number.
    Number,
    /// A macro.
    Annotation,
    /// An argument.
    Argument,
    /// A mutable argument.
    ArgumentMutable,
    /// A local variable.
    Local,
    /// A mutable local variable.
    LocalMutable,
    /// An operator.
    Operator,
    /// A structure.
    Structure,
    /// A class.
    Class,
    /// A union.
    Union,
    /// A disjoint union.
    UnionDisjoint,
    /// A variant.
    Variant,
    /// An interface.
    Interface,
    /// A trait.
    Trait,
}
