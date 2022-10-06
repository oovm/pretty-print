use alloc::{borrow::Cow, string::String};
use core::{
    borrow::Cow,
    ops::{Deref, Range},
    string::String,
};
use pretty::{
    termcolor::{Buffer, Color, ColorSpec},
    Arena, DocAllocator, DocBuilder, Pretty,
};
use crate::providers::{PrettyProvider, PrettyTree};

pub trait ValkyrieNode {
    fn get_range(&self) -> Range<u32>;
    // fn mut_range(&mut self) -> &mut Range<u32>;
}

pub trait PrettyPrint {
    fn build<'a>(&self, allocator: &'a PrettyProvider<'a>) -> PrettyTree<'a>;
    fn pretty_string(&self, width: usize) -> String {
        let arena = PrettyProvider::new();
        let mut buffer = Buffer::ansi();
        if let Err(e) = self.build(&arena).render(width, &mut buffer) {
            return format!("Error: {}", e);
        }
        unsafe { String::from_utf8_unchecked(buffer.into_inner()) }
    }
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
