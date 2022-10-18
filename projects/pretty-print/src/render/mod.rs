use alloc::rc::Rc;
use std::{cmp, fmt, io};

#[cfg(feature = "termcolor")]
use termcolor::{ColorSpec, WriteColor};

use crate::{DocumentTree};

/// Trait representing the operations necessary to render a document
pub trait Render {
    type Error;

    fn write_str(&mut self, s: &str) -> Result<usize, Self::Error>;

    fn write_str_all(&mut self, mut s: &str) -> Result<(), Self::Error> {
        while !s.is_empty() {
            let count = self.write_str(s)?;
            s = &s[count..];
        }
        Ok(())
    }

    fn fail_doc(&self) -> Self::Error;
}

/// Writes to something implementing `std::io::Write`
pub struct IoWrite<W> {
    upstream: W,
}

impl<W> IoWrite<W> {
    pub fn new(upstream: W) -> IoWrite<W> {
        IoWrite { upstream }
    }
}

impl<W> Render for IoWrite<W>
    where
        W: io::Write,
{
    type Error = io::Error;

    fn write_str(&mut self, s: &str) -> io::Result<usize> {
        self.upstream.write(s.as_bytes())
    }

    fn write_str_all(&mut self, s: &str) -> io::Result<()> {
        self.upstream.write_all(s.as_bytes())
    }

    fn fail_doc(&self) -> Self::Error {
        io::Error::new(io::ErrorKind::Other, "Document failed to render")
    }
}

/// Writes to something implementing `std::fmt::Write`
pub struct FmtWrite<W> {
    upstream: W,
}

impl<W> FmtWrite<W> {
    pub fn new(upstream: W) -> FmtWrite<W> {
        FmtWrite { upstream }
    }
}

impl<W> Render for FmtWrite<W>
    where
        W: fmt::Write,
{
    type Error = fmt::Error;

    fn write_str(&mut self, s: &str) -> Result<usize, fmt::Error> {
        self.write_str_all(s).map(|_| s.len())
    }

    fn write_str_all(&mut self, s: &str) -> fmt::Result {
        self.upstream.write_str(s)
    }

    fn fail_doc(&self) -> Self::Error {
        fmt::Error
    }
}

/// Trait representing the operations necessary to write an annotated document.
pub trait RenderAnnotated: Render {
    fn push_annotation<'a, 'b>(&'a mut self, annotation: &'b ColorSpec) -> Result<(), Self::Error>;
    fn pop_annotation(&mut self) -> Result<(), Self::Error>;
}

impl<W> RenderAnnotated for IoWrite<W>
    where
        W: io::Write,
{
    fn push_annotation(&mut self, _: &ColorSpec) -> Result<(), Self::Error> {
        Ok(())
    }

    fn pop_annotation(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}

impl<W> RenderAnnotated for FmtWrite<W>
    where
        W: fmt::Write,
{
    fn push_annotation(&mut self, _: &ColorSpec) -> Result<(), Self::Error> {
        Ok(())
    }

    fn pop_annotation(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}

pub struct TermColored<W> {
    color_stack: Vec<ColorSpec>,
    upstream: W,
}

#[cfg(feature = "termcolor")]
impl<W> TermColored<W> {
    pub fn new(upstream: W) -> TermColored<W> {
        TermColored {
            color_stack: Vec::new(),
            upstream,
        }
    }
}

#[cfg(feature = "termcolor")]
impl<W> Render for TermColored<W>
    where
        W: io::Write,
{
    type Error = io::Error;

    fn write_str(&mut self, s: &str) -> io::Result<usize> {
        self.upstream.write(s.as_bytes())
    }

    fn write_str_all(&mut self, s: &str) -> io::Result<()> {
        self.upstream.write_all(s.as_bytes())
    }

    fn fail_doc(&self) -> Self::Error {
        io::Error::new(io::ErrorKind::Other, "Document failed to render")
    }
}

impl<W> RenderAnnotated for TermColored<W>
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

enum Annotation<'a, A> {
    Push(&'a A),
    Pop,
}

struct BufferWrite<'a> {
    buffer: String,
    annotations: Vec<(usize, Annotation<'a, ColorSpec>)>,
}

impl<'a> BufferWrite<'a> {
    fn new() -> Self {
        BufferWrite {
            buffer: String::new(),
            annotations: Vec::new(),
        }
    }

    fn render<W>(&mut self, render: &mut W) -> Result<(), W::Error>
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
                Annotation::Push(a) => render.push_annotation(a)?,
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

impl Render for BufferWrite<'_> {
    type Error = ();

    fn write_str(&mut self, s: &str) -> Result<usize, Self::Error> {
        self.buffer.push_str(s);
        Ok(s.len())
    }

    fn write_str_all(&mut self, s: &str) -> Result<(), Self::Error> {
        self.buffer.push_str(s);
        Ok(())
    }

    fn fail_doc(&self) -> Self::Error {}
}

impl RenderAnnotated for BufferWrite<'_> {
    fn push_annotation<'a, 'b>(&'a mut self, annotation: &'b ColorSpec) -> Result<(), Self::Error> {
        todo!()
        // self.annotations
        //     .push((self.buffer.len(), Annotation::Push(annotation)));
        // Ok(())
    }


    fn pop_annotation(&mut self) -> Result<(), Self::Error> {
        self.annotations.push((self.buffer.len(), Annotation::Pop));
        Ok(())
    }
}

macro_rules! make_spaces {
    () => { "" };
    ($s: tt $($t: tt)*) => { concat!("          ", make_spaces!($($t)*)) };
}

pub(crate) const SPACES: &str = make_spaces!(,,,,,,,,,,);

fn append_docs2(
    ldoc: Rc<DocumentTree>,
    rdoc: Rc<DocumentTree>,
    mut consumer: impl FnMut(Rc<DocumentTree>),
) -> Rc<DocumentTree>

{
    let d = append_docs(rdoc, &mut consumer);
    consumer(d);
    append_docs(ldoc, &mut consumer)
}

fn append_docs(
    mut doc: Rc<DocumentTree>,
    consumer: &mut impl FnMut(Rc<DocumentTree>),
) -> Rc<DocumentTree>
{
    loop {
        // Since appended documents often appear in sequence on the left side we
        // gain a slight performance increase by batching these pushes (avoiding
        // to push and directly pop `Append` documents)
        match doc.as_ref() {
            DocumentTree::Append { base, rest } => {
                let d = append_docs(base.clone(), consumer);
                consumer(d);
                doc = rest.clone();
            }
            _ => return doc,
        }
    }
}

pub fn best<'a, W>(doc: Rc<DocumentTree>, width: usize, out: &mut W) -> Result<(), W::Error>
    where
        W: RenderAnnotated,
            W: ?Sized,
{
    Best {
        pos: 0,
        bcmds: vec![(0, Mode::Break, doc)],
        fcmds: vec![],
        annotation_levels: vec![],
        width,
    }
        .best(0, out)?;

    Ok(())
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Mode {
    Break,
    Flat,
}

type Cmd = (usize, Mode, Rc<DocumentTree>);

fn write_newline<W>(ind: usize, out: &mut W) -> Result<(), W::Error>
    where
        W: ?Sized + Render,
{
    out.write_str_all("\n")?;
    write_spaces(ind, out)
}

fn write_spaces<W>(spaces: usize, out: &mut W) -> Result<(), W::Error>
    where
        W: ?Sized + Render,
{
    let mut inserted = 0;
    while inserted < spaces {
        let insert = cmp::min(SPACES.len(), spaces - inserted);
        inserted += out.write_str(&SPACES[..insert])?;
    }

    Ok(())
}

struct Best
{
    pos: usize,
    bcmds: Vec<Cmd>,
    fcmds: Vec<Rc<DocumentTree>>,
    annotation_levels: Vec<usize>,
    width: usize,
}

impl Best
{
    fn fitting(&mut self, next: Rc<DocumentTree>, mut pos: usize, ind: usize) -> bool {
        let mut bidx = self.bcmds.len();
        self.fcmds.clear(); // clear from previous calls from best
        self.fcmds.push(next);

        let mut mode = Mode::Flat;
        loop {
            let mut doc = match self.fcmds.pop() {
                None => {
                    if bidx == 0 {
                        // All commands have been processed
                        return true;
                    } else {
                        bidx -= 1;
                        mode = Mode::Break;
                        self.bcmds[bidx].2.clone()
                    }
                }
                Some(cmd) => cmd,
            };

            loop {
                match doc.as_ref() {
                    DocumentTree::Nil => {}
                    DocumentTree::Append { base, rest } => {
                        doc = append_docs2(base.clone(), rest.clone(), |doc| self.fcmds.push(doc));
                        continue;
                    }
                    // Newlines inside the group makes it not fit, but those outside lets it
                    // fit on the current line
                    DocumentTree::Hardline => return mode == Mode::Break,
                    DocumentTree::RenderLen { len, doc: _ } => {
                        pos += len;
                        if pos > self.width {
                            return false;
                        }
                    }
                    DocumentTree::StaticText(str) => {
                        pos += str.len();
                        if pos > self.width {
                            return false;
                        }
                    }
                    DocumentTree::Text(ref str) => {
                        pos += str.len();
                        if pos > self.width {
                            return false;
                        }
                    }
                    // Doc::SmallText(ref str) => {
                    //     pos += str.len();
                    //     if pos > self.width {
                    //         return false;
                    //     }
                    // }
                    DocumentTree::FlatAlt { block: flat, inline } => {
                        doc = match mode {
                            Mode::Break => flat.clone(),
                            Mode::Flat => inline.clone(),
                        };
                        continue;
                    }

                    DocumentTree::Column { column } => {
                        doc = Rc::new(column(pos));
                        continue;
                    }
                    DocumentTree::Nesting { nesting } => {
                        doc = Rc::new(nesting(ind));
                        continue;
                    }
                    DocumentTree::Nest { space: _, doc: next }
                    | DocumentTree::Group { items: next }
                    | DocumentTree::Annotated { color: _, doc: next }
                    | DocumentTree::Union { left: _, right: next } => {
                        doc = next.clone();
                        continue;
                    }
                    DocumentTree::Fail => return false,
                }
                break;
            }
        }
    }

    fn best<W>(&mut self, top: usize, out: &mut W) -> Result<bool, W::Error>
        where
            W: RenderAnnotated,
            W: ?Sized,
    {
        let mut fits = true;

        while top < self.bcmds.len() {
            let mut cmd = self.bcmds.pop().unwrap();
            loop {
                let (ind, mode, doc) = cmd;
                match doc.as_ref() {
                    DocumentTree::Nil => {}
                    DocumentTree::Append { base, rest } => {
                        cmd.2 = append_docs2(base.clone(), rest.clone(), |doc| self.bcmds.push((ind, mode, doc)));
                        continue;
                    }
                    DocumentTree::FlatAlt { block, inline } => {
                        cmd.2 = match mode {
                            Mode::Break => block.clone(),
                            Mode::Flat => inline.clone(),
                        };
                        continue;
                    }
                    DocumentTree::Group { items } => {
                        if let Mode::Break = mode {
                            if self.fitting(items.clone(), self.pos, ind) {
                                cmd.1 = Mode::Flat;
                            }
                        }
                        cmd.2 = doc;
                        continue;
                    }
                    DocumentTree::Nest { space, doc } => {
                        // Once https://doc.rust-lang.org/std/primitive.usize.html#method.saturating_add_signed is stable
                        // this can be replaced
                        let new_ind = if *space >= 0 {
                            ind.saturating_add(*space as usize)
                        } else {
                            ind.saturating_sub(space.unsigned_abs())
                        };
                        cmd = (new_ind, mode, doc.clone());
                        continue;
                    }
                    DocumentTree::Hardline => {
                        // The next document may have different indentation so we should use it if
                        // we can
                        if let Some(next) = self.bcmds.pop() {
                            write_newline(next.0, out)?;
                            self.pos = next.0;
                            cmd = next;
                            continue;
                        } else {
                            write_newline(ind, out)?;
                            self.pos = ind;
                        }
                    }
                    DocumentTree::RenderLen { len, doc } => match doc.as_ref() {
                        DocumentTree::Text(s) => {
                            out.write_str_all(s)?;
                            self.pos += len;
                            fits &= self.pos <= self.width;
                        }
                        DocumentTree::StaticText(s) => {
                            out.write_str_all(s)?;
                            self.pos += len;
                            fits &= self.pos <= self.width;
                        }
                        // Doc::SmallText(ref s) => {
                        //     out.write_str_all(s)?;
                        //     self.pos += len;
                        //     fits &= self.pos <= self.width;
                        // }
                        _ => unreachable!(),
                    },
                    DocumentTree::Text(ref s) => {
                        out.write_str_all(s)?;
                        self.pos += s.len();
                        fits &= self.pos <= self.width;
                    }
                    DocumentTree::StaticText(s) => {
                        out.write_str_all(s)?;
                        self.pos += s.len();
                        fits &= self.pos <= self.width;
                    }
                    // Doc::SmallText(ref s) => {
                    //     out.write_str_all(s)?;
                    //     self.pos += s.len();
                    //     fits &= self.pos <= self.width;
                    // }
                    DocumentTree::Annotated { color, doc } => {
                        out.push_annotation(&color)?;
                        self.annotation_levels.push(self.bcmds.len());
                        cmd.2 = doc.clone();
                        continue;
                    }
                    DocumentTree::Union { left, right } => {
                        let pos = self.pos;
                        let annotation_levels = self.annotation_levels.len();
                        let bcmds = self.bcmds.len();

                        self.bcmds.push((ind, mode, left.clone()));

                        let mut buffer = BufferWrite::new();

                        match self.best(bcmds, &mut buffer) {
                            Ok(true) => buffer.render(out)?,
                            Ok(false) | Err(()) => {
                                self.pos = pos;
                                self.bcmds.truncate(bcmds);
                                self.annotation_levels.truncate(annotation_levels);
                                cmd.2 = right.clone();
                                continue;
                            }
                        }
                    }
                    DocumentTree::Column { column } => {
                        cmd.2 = Rc::new(column(self.pos));
                        continue;
                    }
                    DocumentTree::Nesting { nesting } => {
                        cmd.2 = Rc::new(nesting(self.pos));
                        continue;
                    }
                    DocumentTree::Fail => return Err(out.fail_doc()),
                }

                break;
            }
            while self.annotation_levels.last() == Some(&self.bcmds.len()) {
                self.annotation_levels.pop();
                out.pop_annotation()?;
            }
        }

        Ok(fits)
    }
}
