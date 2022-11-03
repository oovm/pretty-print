use crate::{render::Annotation, Render, RenderAnnotated};
use alloc::rc::Rc;
use color_ansi::AnsiStyle;
use core::fmt::{Debug, Formatter};

/// Writes to something implementing `std::fmt::Write`
pub struct FmtWrite<W> {
    upstream: W,
}
/// Represents a terminal writer.
#[derive(Debug)]
pub struct BufferWrite {
    buffer: String,
    annotations: Vec<(usize, Annotation<AnsiStyle>)>,
}

impl BufferWrite {
    /// Creates a new terminal writer.
    pub fn new(capacity: usize) -> Self {
        BufferWrite { buffer: String::with_capacity(capacity), annotations: Vec::new() }
    }
    /// Creates a new terminal writer.
    pub fn render<W>(&mut self, render: &mut W) -> Result<(), W::Error>
    where
        W: RenderAnnotated,
        W: ?Sized,
    {
        let mut start = 0;
        for (end, annotation) in &self.annotations {
            let s = &self.buffer[start..*end];
            if !s.is_empty() {
                render.write_str_all(s)?;
            }
            start = *end;
            match annotation {
                Annotation::Push(a) => render.push_annotation(a.clone())?,
                Annotation::Pop => render.pop_annotation()?,
            }
        }
        let s = &self.buffer[start..];
        if !s.is_empty() {
            render.write_str_all(s)?;
        }
        Ok(())
    }
}

impl Render for BufferWrite {
    type Error = core::fmt::Error;

    fn write_str(&mut self, s: &str) -> Result<usize, Self::Error> {
        self.buffer.push_str(s);
        Ok(s.len())
    }

    fn write_str_all(&mut self, s: &str) -> Result<(), Self::Error> {
        self.buffer.push_str(s);
        Ok(())
    }

    fn fail_doc(&self) -> Self::Error {
        core::fmt::Error::default()
    }
}

impl<W> RenderAnnotated for FmtWrite<W>
where
    W: core::fmt::Write,
{
    fn push_annotation(&mut self, _: Rc<AnsiStyle>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn pop_annotation(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}

impl RenderAnnotated for BufferWrite {
    fn push_annotation(&mut self, annotation: Rc<AnsiStyle>) -> Result<(), Self::Error> {
        self.annotations.push((self.buffer.len(), Annotation::Push(annotation)));
        Ok(())
    }

    fn pop_annotation(&mut self) -> Result<(), Self::Error> {
        self.annotations.push((self.buffer.len(), Annotation::Pop));
        Ok(())
    }
}

impl<W> Debug for FmtWrite<W> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("IoWrite").finish()
    }
}

impl<W> FmtWrite<W> {
    /// Create a new `FmtWrite` from something implementing `std::fmt::Write`
    pub fn new(upstream: W) -> FmtWrite<W> {
        FmtWrite { upstream }
    }
}

impl<W> Render for FmtWrite<W>
where
    W: core::fmt::Write,
{
    type Error = core::fmt::Error;

    fn write_str(&mut self, s: &str) -> Result<usize, core::fmt::Error> {
        self.write_str_all(s).map(|_| s.len())
    }

    fn write_str_all(&mut self, s: &str) -> core::fmt::Result {
        self.upstream.write_str(s)
    }

    fn fail_doc(&self) -> Self::Error {
        core::fmt::Error
    }
}
