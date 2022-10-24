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
        inline_join: DocumentTree,
        block_join: DocumentTree,
    ) -> DocumentTree
    where
        I: PrettyPrint,
    {
        let mut output = DocumentSequence::new(5);
        if self.head_space {
            output.push(" ");
        }
        output.push(self.bracket_l);
        // inline
        let mut inline = DocumentSequence::new(3);
        inline.push(" ");
        inline.push(allocator.join(items, inline_join));
        inline.push(" ");
        // block
        let mut block = DocumentSequence::new(3);
        block.push(DocumentTree::Hardline);
        block.push(allocator.join(items, block_join).indent(4));
        block.push(DocumentTree::Hardline);
        //
        output.push(block.flat_alt(inline));
        output.push(self.bracket_r);
        output.into()
    }
}
