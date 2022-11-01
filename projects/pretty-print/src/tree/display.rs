use super::*;

impl Default for PrettyTree {
    fn default() -> Self {
        Self::Nil
    }
}

impl Clone for PrettyTree {
    fn clone(&self) -> Self {
        match self {
            Self::Nil => Self::Nil,
            Self::Hardline => Self::Hardline,
            Self::Text(s) => Self::Text(s.clone()),
            Self::StaticText(s) => Self::StaticText(*s),
            Self::Annotated { color, doc } => Self::Annotated { color: color.clone(), doc: doc.clone() },
            Self::Append { lhs, rhs } => Self::Append { lhs: lhs.clone(), rhs: rhs.clone() },
            Self::Group { items } => Self::Group { items: items.clone() },
            Self::MaybeInline { block, inline } => Self::MaybeInline { block: block.clone(), inline: inline.clone() },
            Self::Nest { space, doc } => Self::Nest { space: *space, doc: doc.clone() },
            Self::RenderLen { len, doc } => Self::RenderLen { len: *len, doc: doc.clone() },
            Self::Union { left, right } => Self::Union { left: left.clone(), right: right.clone() },
            Self::Column { function: column } => Self::Column { function: column.clone() },
            Self::Nesting { function: nesting } => Self::Nesting { function: nesting.clone() },
            Self::Fail => Self::Fail,
        }
    }
}

impl Debug for PrettyTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        let is_line = |doc: &PrettyTree| match doc {
            PrettyTree::MaybeInline { block: flat, inline: alt } => {
                matches!((&**flat, &**alt), (PrettyTree::Hardline, PrettyTree::StaticText(" ")))
            }
            _ => false,
        };
        let is_line_ = |doc: &PrettyTree| match doc {
            PrettyTree::MaybeInline { block: flat, inline: alt } => {
                matches!((&**flat, &**alt), (PrettyTree::Hardline, PrettyTree::Nil))
            }
            _ => false,
        };
        match self {
            PrettyTree::Nil => f.debug_tuple("Nil").finish(),
            PrettyTree::Append { lhs: _, rhs: _ } => {
                let mut f = f.debug_list();
                append_docs(self, &mut |doc| {
                    f.entry(doc);
                });
                f.finish()
            }
            _ if is_line(self) => f.debug_tuple("Line").finish(),
            _ if is_line_(self) => f.debug_tuple("Line?").finish(),
            PrettyTree::MaybeInline { block, inline } => f.debug_tuple("FlatAlt").field(block).field(inline).finish(),
            PrettyTree::Group { items } => {
                if is_line(self) {
                    return f.debug_tuple("SoftLine").finish();
                }
                if is_line_(self) {
                    return f.debug_tuple("SoftLine?").finish();
                }
                f.debug_tuple("Group").field(items).finish()
            }
            PrettyTree::Nest { space, doc } => f.debug_tuple("Nest").field(&space).field(doc).finish(),
            PrettyTree::Hardline => f.debug_tuple("Hardline").finish(),
            PrettyTree::RenderLen { doc, .. } => doc.fmt(f),
            PrettyTree::Text(s) => Debug::fmt(s, f),
            PrettyTree::StaticText(s) => Debug::fmt(s, f),
            PrettyTree::Annotated { color, doc } => f.debug_tuple("Annotated").field(color).field(doc).finish(),
            PrettyTree::Union { left, right } => f.debug_tuple("Union").field(left).field(right).finish(),
            PrettyTree::Column { .. } => f.debug_tuple("Column(..)").finish(),
            PrettyTree::Nesting { .. } => f.debug_tuple("Nesting(..)").finish(),
            PrettyTree::Fail => f.debug_tuple("Fail").finish(),
        }
    }
}

fn append_docs(mut doc: &PrettyTree, consumer: &mut impl FnMut(&PrettyTree)) {
    loop {
        match doc {
            PrettyTree::Append { lhs, rhs } => {
                append_docs(lhs, consumer);
                doc = rhs;
            }
            _ => break consumer(doc),
        }
    }
}
