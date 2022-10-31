use crate::{Render, RenderAnnotated};
use alloc::rc::Rc;
use color_ansi::{AnsiAbility, AnsiStyle, AnsiWriter};
use core::fmt::{Debug, Formatter};
use std::io::{Error, ErrorKind, Write};
/// Represents a terminal writer.
pub struct TerminalWriter<W> {
    color_stack: Vec<Rc<AnsiStyle>>,
    upstream: AnsiWriter<W>,
}

impl<W> Debug for TerminalWriter<W> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("TerminalWriter").finish()
    }
}

impl<W: Write> TerminalWriter<W> {
    /// Creates a new terminal writer.
    pub fn new(upstream: W) -> Self {
        TerminalWriter { color_stack: Vec::new(), upstream: AnsiWriter::new(upstream) }
    }
    /// Creates a new terminal writer with a specific color.
    pub fn with_color(mut self, color: AnsiAbility) -> Self {
        self.upstream.set_ability(color);
        self
    }
}

impl<W> Render for TerminalWriter<W>
where
    W: Write,
{
    type Error = Error;

    fn write_str(&mut self, s: &str) -> std::io::Result<usize> {
        self.upstream.write(s.as_bytes())
    }

    fn write_str_all(&mut self, s: &str) -> std::io::Result<()> {
        self.upstream.write_all(s.as_bytes())
    }

    fn fail_doc(&self) -> Self::Error {
        Error::new(ErrorKind::Other, "Document failed to render")
    }
}

impl<W: Write> RenderAnnotated for TerminalWriter<W> {
    fn push_annotation(&mut self, color: Rc<AnsiStyle>) -> Result<(), Self::Error> {
        self.color_stack.push(color.clone());
        self.upstream.set_style(&color)
    }

    fn pop_annotation(&mut self) -> Result<(), Self::Error> {
        self.color_stack.pop();
        match self.color_stack.last() {
            Some(previous) => self.upstream.set_style(previous),
            None => self.upstream.reset_style(),
        }
    }
}
