
use std::{cmp, fmt, io};
use termcolor::{ColorSpec, WriteColor};
use crate::DocumentTree;

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
        TermColored { color_stack: Vec::new(), upstream }
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
        BufferWrite { buffer: String::new(), annotations: Vec::new() }
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

fn append_docs2<'a>(
    ldoc: &'a DocumentTree,
    rdoc: &'a DocumentTree,
    mut consumer: impl FnMut(&'a DocumentTree),
) -> &'a DocumentTree {
    let d = append_docs(&rdoc, &mut consumer);
    consumer(d);
    append_docs(&ldoc, &mut consumer)
}

fn append_docs<'a>(mut doc: &'a DocumentTree, consumer: &mut impl FnMut(&'a DocumentTree)) -> &'a DocumentTree {
    loop {
        // Since appended documents often appear in sequence on the left side we
        // gain a slight performance increase by batching these pushes (avoiding
        // to push and directly pop `Append` documents)
        match doc {
            DocumentTree::Append { lhs: base, rhs: rest } => {
                let d = append_docs(base, consumer);
                consumer(d);
                doc = rest;
            }
            _ => return doc,
        }
    }
}

pub fn best<'a, W>(doc: &'a DocumentTree, width: usize, out: &mut W) -> Result<(), W::Error>
where
    W: RenderAnnotated,
    W: ?Sized,
{
    Best {
        pos: 0,
        back_cmds: vec![RenderCommand { indent: 0, mode: Mode::Break, node: doc }],
        front_cmds: vec![],
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

struct RenderCommand<'a> {
    indent: usize,
    mode: Mode,
    node: &'a DocumentTree,
}

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

struct Best<'a> {
    pos: usize,
    back_cmds: Vec<RenderCommand<'a>>,
    front_cmds: Vec<&'a DocumentTree>,
    annotation_levels: Vec<usize>,
    width: usize,
}

impl<'a> Best<'a> {
    fn fitting(&mut self, next: &'a DocumentTree, mut pos: usize, ind: usize) -> bool {
        let mut bidx = self.back_cmds.len();
        self.front_cmds.clear(); // clear from previous calls from best
        self.front_cmds.push(next);

        let mut mode = Mode::Flat;
        loop {
            let mut doc = match self.front_cmds.pop() {
                None => {
                    if bidx == 0 {
                        // All commands have been processed
                        return true;
                    }
                    else {
                        bidx -= 1;
                        mode = Mode::Break;
                        self.back_cmds[bidx].node
                    }
                }
                Some(cmd) => cmd,
            };

            loop {
                match doc {
                    DocumentTree::Nil => {}
                    DocumentTree::Append { lhs: base, rhs: rest } => {
                        doc = append_docs2(base, rest, |doc| self.front_cmds.push(doc));
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
                            Mode::Break => flat,
                            Mode::Flat => inline,
                        };
                        continue;
                    }

                    DocumentTree::Column { column } => {
                        todo!();
                        // doc = &column(pos);
                        continue;
                    }
                    DocumentTree::Nesting { nesting } => {
                        todo!();
                        // doc = &nesting(ind);
                        continue;
                    }
                    DocumentTree::Nest { space: _, doc: next }
                    | DocumentTree::Group { items: next }
                    | DocumentTree::Annotated { color: _, doc: next }
                    | DocumentTree::Union { left: _, right: next } => {
                        doc = next;
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

        while top < self.back_cmds.len() {
            let mut cmd = self.back_cmds.pop().unwrap();
            loop {
                let RenderCommand { indent: ind, mode, node: doc } = cmd;
                match doc {
                    DocumentTree::Nil => {}
                    DocumentTree::Append { lhs, rhs } => {
                        println!("lhs: {:?}", lhs);
                        println!("rhs: {:?}", rhs);

                        cmd.node =
                            append_docs2(lhs, rhs, |doc| self.back_cmds.push(RenderCommand { indent: ind, mode, node: doc }));
                        continue;
                    }
                    DocumentTree::FlatAlt { block, inline } => {
                        cmd.node = match mode {
                            Mode::Break => block,
                            Mode::Flat => inline,
                        };
                        continue;
                    }
                    DocumentTree::Group { items } => {
                        if let Mode::Break = mode {
                            if self.fitting(items, self.pos, ind) {
                                cmd.mode = Mode::Flat;
                            }
                        }
                        cmd.node = doc;
                        continue;
                    }
                    DocumentTree::Nest { space, doc } => {
                        // Once https://doc.rust-lang.org/std/primitive.usize.html#method.saturating_add_signed is stable
                        // this can be replaced
                        let new_ind = if *space >= 0 {
                            ind.saturating_add(*space as usize)
                        }
                        else {
                            ind.saturating_sub(space.unsigned_abs())
                        };
                        cmd = RenderCommand { indent: new_ind, mode, node: doc };
                        continue;
                    }
                    DocumentTree::Hardline => {
                        // The next document may have different indentation so we should use it if
                        // we can
                        match self.back_cmds.pop() {
                            Some(next) => {
                                write_newline(next.indent, out)?;
                                self.pos = next.indent;
                                cmd = next;
                                continue;
                            }
                            None => {
                                write_newline(ind, out)?;
                                self.pos = ind;
                            }
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
                        self.annotation_levels.push(self.back_cmds.len());
                        cmd.node = doc;
                        continue;
                    }
                    DocumentTree::Union { left, right } => {
                        let pos = self.pos;
                        let annotation_levels = self.annotation_levels.len();
                        let bcmds = self.back_cmds.len();

                        self.back_cmds.push(RenderCommand { indent: ind, mode, node: left });

                        let mut buffer = BufferWrite::new();

                        match self.best(bcmds, &mut buffer) {
                            Ok(true) => buffer.render(out)?,
                            Ok(false) | Err(()) => {
                                self.pos = pos;
                                self.back_cmds.truncate(bcmds);
                                self.annotation_levels.truncate(annotation_levels);
                                cmd.node = right;
                                continue;
                            }
                        }
                    }
                    DocumentTree::Column { column } => {
                        todo!();
                        // cmd.node = &column(self.pos);
                        continue;
                    }
                    DocumentTree::Nesting { nesting } => {
                        todo!();
                        // cmd.node = &nesting(self.pos);
                        continue;
                    }
                    DocumentTree::Fail => return Err(out.fail_doc()),
                }

                break;
            }
            while self.annotation_levels.last() == Some(&self.back_cmds.len()) {
                self.annotation_levels.pop();
                out.pop_annotation()?;
            }
        }

        Ok(fits)
    }
}
