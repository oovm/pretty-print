use super::*;

/// Marker trait for types that can be pretty printed.
pub trait PrettyPrint {
    /// Build a pretty tree for this type.
    fn pretty(&self, theme: &PrettyProvider) -> PrettyTree;
    /// Get a pretty string for this type.
    fn pretty_string(&self, theme: &PrettyProvider) -> String {
        let mut buffer = String::new();
        if let Err(e) = self.pretty(&theme).render_fmt(theme.get_width(), &mut buffer) {
            panic!("Error: {}", e);
        }
        buffer
    }
    /// Print a pretty string for this type.
    fn pretty_colorful(&self, theme: &PrettyProvider) -> String {
        let mut buffer = vec![];
        if let Err(e) = self.pretty(&theme).render_colored(theme.get_width(), &mut buffer) {
            panic!("Error: {}", e);
        }
        match String::from_utf8(buffer) {
            Ok(s) => s,
            Err(e) => panic!("Error: {}", e),
        }
    }
}

impl PrettyPrint for PrettyTree {
    fn pretty(&self, _: &PrettyProvider) -> PrettyTree {
        self.clone()
    }
}
impl PrettyPrint for &'static str {
    fn pretty(&self, _: &PrettyProvider) -> PrettyTree {
        PrettyTree::StaticText(*self)
    }
}
