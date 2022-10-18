use crate::providers::{PrettyProvider};
use alloc::string::String;
use termcolor::Buffer;
use crate::DocumentTree;


/// Marker trait for types that can be pretty printed.
pub trait PrettyPrint {
    /// Build a pretty tree for this type.
    fn build<'a>(&self, allocator: &'a PrettyProvider) -> DocumentTree;
    /// Get a pretty string for this type.
    fn pretty_string(&self, width: usize) -> String {
        let arena = PrettyProvider::new();
        let mut buffer = Buffer::ansi();
        if let Err(e) = self.build(&arena).render(width, &mut buffer) {
            return alloc::format!("Error: {}", e);
        }
        unsafe { String::from_utf8_unchecked(buffer.into_inner()) }
    }
    /// Print a pretty string for this type.
    #[cfg(feature = "std")]
    fn pretty_print(&self, width: usize) {
        let arena = PrettyProvider::new();
        let mut buffer = Buffer::ansi();
        match self.build(&arena).render_colored(width, &mut buffer) {
            Ok(_) => {
                println!("{}", unsafe { String::from_utf8_unchecked(buffer.into_inner()) });
            }
            Err(e) => {
                eprintln!("Error: {}", e);
            }
        }
    }
}
