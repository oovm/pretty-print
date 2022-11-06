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
pub struct HardBlock {
    /// The indentation of the soft block
    pub indent: usize,
    /// The left hand side of the soft block
    pub lhs: &'static str,
    /// The right hand side of the soft block
    pub rhs: &'static str,
    /// The joint node of the soft block
    pub joint: PrettyTree,
}

impl HardBlock {
    /// Build a new soft block
    pub fn new(lhs: &'static str, rhs: &'static str) -> Self {
        Self { lhs, rhs, indent: 4, joint: PrettyTree::line_or_space() }
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

impl HardBlock {
    /// Join a slice of pretty printables with the soft block
    pub fn join_slice<T: PrettyPrint>(&self, slice: &[T], theme: &PrettyProvider) -> PrettyTree {
        let mut outer = PrettySequence::new(5);
        outer += self.lhs;
        outer += PrettyTree::Hardline;
        let mut inner = PrettySequence::new(slice.len() * 2);
        for (idx, term) in slice.iter().enumerate() {
            if idx != 0 {
                inner += self.joint.clone();
            }
            inner += term.pretty(theme);
        }
        outer += inner.indent(self.indent);
        outer += PrettyTree::Hardline;
        outer += self.rhs;
        outer.into()
    }
}
