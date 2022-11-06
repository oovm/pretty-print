use super::*;

/// A soft block is a block that is not required to be on a new line.
///
/// ```vk
/// {a, b, c}
///
/// {
///     a,
///     b,
/// }
/// ```
#[derive(Clone, Debug)]
pub struct SoftBlock {
    /// The indentation of the soft block
    pub indent: usize,
    /// The left hand side of the soft block
    pub lhs: &'static str,
    /// The right hand side of the soft block
    pub rhs: &'static str,
    /// The joint node of the soft block
    pub joint: PrettyTree,
    /// The tail node of the soft block
    pub tail: PrettyTree,
}

impl SoftBlock {
    /// Build a new soft block
    pub fn new(lhs: &'static str, rhs: &'static str) -> Self {
        Self { lhs, rhs, indent: 4, joint: PrettyTree::line_or_space(), tail: PrettyTree::Nil }
    }
    /// Build a new soft block with the tuple syntax
    pub fn tuple() -> Self {
        Self::new("(", ")")
    }
    /// Build a new soft block with the parentheses syntax
    pub fn parentheses() -> Self {
        Self::new("(", ")")
    }
    /// Build a new soft block with the brackets syntax
    pub fn brackets() -> Self {
        Self::new("[", "]")
    }
    /// Build a new soft block with the curly braces syntax
    pub fn curly_braces() -> Self {
        Self::new("{", "}")
    }
    /// Set the joint node of the soft block
    pub fn with_joint(self, joint: PrettyTree) -> Self {
        Self { joint, ..self }
    }
}

impl SoftBlock {
    /// Join a slice of pretty printables with the soft block
    pub fn join_slice<T: PrettyPrint>(&self, slice: &[T], theme: &PrettyProvider) -> PrettyTree {
        let mut outer = PrettySequence::new(5);
        outer += self.lhs;
        outer += PrettyTree::line_or_space();
        let mut inner = PrettySequence::new(slice.len() * 2);
        for (idx, term) in slice.iter().enumerate() {
            if idx != 0 {
                inner += self.joint.clone();
            }
            inner += term.pretty(theme);
        }
        outer += inner.indent(self.indent);
        outer += PrettyTree::line_or_space();
        outer += self.rhs;
        outer.into()
    }
}
