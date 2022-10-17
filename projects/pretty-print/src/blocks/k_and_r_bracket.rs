use super::*;

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
        allocator: &'a PrettyProvider<'a>,
        inline_join: DocumentTree,
        block_join: DocumentTree,
    ) -> DocumentTree
        where
            I: PrettyPrint,
    {
        let mut output = Vec::with_capacity(5);
        if self.head_space {
            output.push(allocator.space());
        }
        output.push(allocator.text(self.bracket_l));
        // inline
        let mut inline = Vec::with_capacity(3);
        inline.push(allocator.space());
        inline.push(allocator.intersperse(items, inline_join));
        inline.push(allocator.space());
        let inline = allocator.concat(inline);
        // block
        let mut block = Vec::with_capacity(3);
        block.push(allocator.hardline());
        block.push(allocator.intersperse(items, block_join).indent(4));
        block.push(allocator.hardline());
        let block = allocator.concat(block);
        //
        output.push(block.flat_alt(inline));
        output.push(allocator.text(self.bracket_r));
        allocator.concat(output)
    }
}