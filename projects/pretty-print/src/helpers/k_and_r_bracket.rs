use super::*;
use crate::PrettyBuilder;

/// `K & R` style brackets
///
/// ```vk
/// a {}
/// ```
///
/// ```vk
/// a {
///
/// }
/// ```
#[derive(Copy, Clone, Debug)]
pub struct KAndRBracket {
    /// Whether to add a space after the keyword
    pub head_space: bool,
    /// The left bracket
    pub bracket_l: &'static str,
    /// The right bracket
    pub bracket_r: &'static str,
}

impl KAndRBracket {
    /// Build a bracketed block
    pub fn curly_braces() -> Self {
        Self { head_space: true, bracket_l: "{", bracket_r: "}" }
    }
    /// Build a bracketed block
    pub fn build<'a, I>(
        &self,
        items: &[I],
        allocator: &'a PrettyProvider,
        inline_join: PrettyTree,
        block_join: PrettyTree,
    ) -> PrettyTree
    where
        I: PrettyPrint,
    {
        let mut output = PrettySequence::new(5);
        if self.head_space {
            output.push(" ");
        }
        output.push(self.bracket_l);
        // inline
        let mut inline = PrettySequence::new(3);
        inline.push(" ");
        inline.push(allocator.join_slice(items, inline_join));
        inline.push(" ");
        // block
        let mut block = PrettySequence::new(3);
        block.push(PrettyTree::Hardline);
        block.push(allocator.join_slice(items, block_join).indent(4));
        block.push(PrettyTree::Hardline);
        //
        output.push(block.flat_alt(inline));
        output.push(self.bracket_r);
        output.into()
    }
}
