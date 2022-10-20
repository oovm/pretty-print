use std::io::{Error, ErrorKind, Write};
use termcolor::{ColorSpec, WriteColor};
use crate::{Render, RenderAnnotated};

pub struct TerminalWriter<W> {
    color: bool,
    color_stack: Vec<ColorSpec>,
    upstream: W,
}

impl<W> TerminalWriter<W> {
    pub fn new(upstream: W) -> Self {
        TerminalWriter { color: true, color_stack: Vec::new(), upstream }
    }
    pub fn with_color(self, color: bool) -> Self {
        TerminalWriter { color, ..self }
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

impl<W> RenderAnnotated for TerminalWriter<W>
    where
        W: WriteColor,
{
    fn push_annotation(&mut self, color: &ColorSpec) -> Result<(), Self::Error> {
        self.color_stack.push(color.clone());
        self.upstream.set_color(color)
    }

    fn pop_annotation(&mut self) -> Result<(), Self::Error> {
        self.color_stack.pop();
        match self.color_stack.last() {
            Some(previous) => self.upstream.set_color(previous),
            None => self.upstream.reset(),
        }
    }
}
