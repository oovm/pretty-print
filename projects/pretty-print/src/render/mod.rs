use crate::PrettyTree;
use alloc::{rc::Rc, string::String, vec, vec::Vec};
use color_ansi::AnsiStyle;
use core::fmt::{Debug, Display, Formatter};

#[cfg(feature = "std")]
pub mod terminal;

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

/// The given text, which must not contain line breaks.
#[derive(Debug)]
pub struct PrettyFormatter<'a> {
    tree: &'a PrettyTree,
    width: usize,
}

impl<'a> Display for PrettyFormatter<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        self.tree.render_fmt(self.width, f)
    }
}

impl PrettyTree {
    /// Returns a value which implements `std::fmt::Display`
    ///
    /// ```
    /// use pretty::{BoxDoc, Doc};
    /// let doc =
    ///     BoxDoc::<()>::group(BoxDoc::text("hello").append(Doc::line()).append(Doc::text("world")));
    /// assert_eq!(format!("{}", doc.pretty(80)), "hello world");
    /// ```
    #[inline]
    pub fn pretty(&self, width: usize) -> PrettyFormatter {
        PrettyFormatter { tree: self, width }
    }
}

/// Writes to something implementing `std::io::Write`
pub struct IoWrite<W> {
    upstream: W,
}

impl<W> Debug for IoWrite<W> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("IoWrite").finish()
    }
}

impl<W> IoWrite<W> {
    pub fn new(upstream: W) -> IoWrite<W> {
        IoWrite { upstream }
    }
}

#[cfg(feature = "std")]
impl<W> Render for IoWrite<W>
where
    W: std::io::Write,
{
    type Error = std::io::Error;

    fn write_str(&mut self, s: &str) -> std::io::Result<usize> {
        self.upstream.write(s.as_bytes())
    }

    fn write_str_all(&mut self, s: &str) -> std::io::Result<()> {
        self.upstream.write_all(s.as_bytes())
    }

    fn fail_doc(&self) -> Self::Error {
        std::io::Error::new(std::io::ErrorKind::Other, "Document failed to render")
    }
}

/// Writes to something implementing `std::fmt::Write`
pub struct FmtWrite<W> {
    upstream: W,
}

impl<W> Debug for FmtWrite<W> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("IoWrite").finish()
    }
}

impl<W> FmtWrite<W> {
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

/// Trait representing the operations necessary to write an annotated document.
pub trait RenderAnnotated: Render {
    fn push_annotation(&mut self, annotation: Rc<AnsiStyle>) -> Result<(), Self::Error>;
    fn pop_annotation(&mut self) -> Result<(), Self::Error>;
}

#[cfg(feature = "std")]
impl<W> RenderAnnotated for IoWrite<W>
where
    W: std::io::Write,
{
    fn push_annotation(&mut self, _: Rc<AnsiStyle>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn pop_annotation(&mut self) -> Result<(), Self::Error> {
        Ok(())
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

enum Annotation<A> {
    Push(Rc<A>),
    Pop,
}

struct BufferWrite {
    buffer: String,
    annotations: Vec<(usize, Annotation<AnsiStyle>)>,
}

impl BufferWrite {
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

macro_rules! make_spaces {
    () => { "" };
    ($s: tt $($t: tt)*) => { concat!("          ", make_spaces!($($t)*)) };
}

pub(crate) const SPACES: &str = make_spaces!(,,,,,,,,,,);

fn append_docs2(ldoc: Rc<PrettyTree>, rdoc: Rc<PrettyTree>, mut consumer: impl FnMut(Rc<PrettyTree>)) -> Rc<PrettyTree> {
    let d = append_docs(rdoc, &mut consumer);
    consumer(d);
    append_docs(ldoc, &mut consumer)
}

fn append_docs(mut doc: Rc<PrettyTree>, consumer: &mut impl FnMut(Rc<PrettyTree>)) -> Rc<PrettyTree> {
    loop {
        // Since appended documents often appear in sequence on the left side we
        // gain a slight performance increase by batching these pushes (avoiding
        // to push and directly pop `Append` documents)
        match doc.as_ref() {
            PrettyTree::Append { lhs, rhs } => {
                let d = append_docs(rhs.clone(), consumer);
                consumer(d);
                doc = lhs.clone();
            }
            _ => return doc,
        }
    }
}

pub fn best<W>(doc: Rc<PrettyTree>, width: usize, out: &mut W) -> Result<(), W::Error>
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

struct RenderCommand {
    indent: usize,
    mode: Mode,
    node: Rc<PrettyTree>,
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
        let insert = core::cmp::min(SPACES.len(), spaces - inserted);
        inserted += out.write_str(&SPACES[..insert])?;
    }

    Ok(())
}

struct Best {
    pos: usize,
    back_cmds: Vec<RenderCommand>,
    front_cmds: Vec<Rc<PrettyTree>>,
    annotation_levels: Vec<usize>,
    width: usize,
}

impl Best {
    fn fitting(&mut self, next: Rc<PrettyTree>, mut pos: usize, ind: usize) -> bool {
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
                        self.back_cmds[bidx].node.clone()
                    }
                }
                Some(cmd) => cmd,
            };

            loop {
                match doc.as_ref() {
                    PrettyTree::Nil => {}
                    PrettyTree::Append { lhs: base, rhs: rest } => {
                        doc = append_docs2(base.clone(), rest.clone(), |doc| self.front_cmds.push(doc));
                        continue;
                    }
                    // Newlines inside the group makes it not fit, but those outside lets it
                    // fit on the current line
                    PrettyTree::Hardline => return mode == Mode::Break,
                    PrettyTree::RenderLen { len, doc: _ } => {
                        pos += len;
                        if pos > self.width {
                            return false;
                        }
                    }
                    PrettyTree::StaticText(str) => {
                        pos += str.len();
                        if pos > self.width {
                            return false;
                        }
                    }
                    PrettyTree::Text(ref str) => {
                        pos += str.len();
                        if pos > self.width {
                            return false;
                        }
                    }
                    PrettyTree::MaybeInline { block: flat, inline } => {
                        doc = match mode {
                            Mode::Break => flat.clone(),
                            Mode::Flat => inline.clone(),
                        };
                        continue;
                    }

                    PrettyTree::Column { function } => {
                        doc = Rc::new(function(pos));
                        continue;
                    }
                    PrettyTree::Nesting { function } => {
                        doc = Rc::new(function(ind));
                        continue;
                    }
                    PrettyTree::Nest { space: _, doc: next }
                    | PrettyTree::Group { items: next }
                    | PrettyTree::Annotated { color: _, doc: next }
                    | PrettyTree::Union { left: _, right: next } => {
                        doc = next.clone();
                        continue;
                    }
                    PrettyTree::Fail => return false,
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
                match doc.as_ref() {
                    PrettyTree::Nil => {}
                    PrettyTree::Append { lhs, rhs } => {
                        cmd.node = append_docs2(lhs.clone(), rhs.clone(), |doc| {
                            self.back_cmds.push(RenderCommand { indent: ind, mode, node: doc })
                        });
                        continue;
                    }
                    PrettyTree::MaybeInline { block, inline } => {
                        cmd.node = match mode {
                            Mode::Break => block.clone(),
                            Mode::Flat => inline.clone(),
                        };
                        continue;
                    }
                    PrettyTree::Group { items } => {
                        if let Mode::Break = mode {
                            if self.fitting(items.clone(), self.pos, ind) {
                                cmd.mode = Mode::Flat;
                            }
                        }
                        cmd.node = doc;
                        continue;
                    }
                    PrettyTree::Nest { space, doc } => {
                        // Once https://doc.rust-lang.org/std/primitive.usize.html#method.saturating_add_signed is stable
                        // this can be replaced
                        let new_ind = if *space >= 0 {
                            ind.saturating_add(*space as usize)
                        }
                        else {
                            ind.saturating_sub(space.unsigned_abs())
                        };
                        cmd = RenderCommand { indent: new_ind, mode, node: doc.clone() };
                        continue;
                    }
                    PrettyTree::Hardline => {
                        // The next document may have different indentation so we should use it if we can
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
                    PrettyTree::RenderLen { len, doc } => match doc.as_ref() {
                        PrettyTree::Text(s) => {
                            out.write_str_all(s)?;
                            self.pos += len;
                            fits &= self.pos <= self.width;
                        }
                        PrettyTree::StaticText(s) => {
                            out.write_str_all(s)?;
                            self.pos += len;
                            fits &= self.pos <= self.width;
                        }
                        _ => unreachable!(),
                    },
                    PrettyTree::Text(ref s) => {
                        out.write_str_all(s)?;
                        self.pos += s.len();
                        fits &= self.pos <= self.width;
                    }
                    PrettyTree::StaticText(s) => {
                        out.write_str_all(s)?;
                        self.pos += s.len();
                        fits &= self.pos <= self.width;
                    }
                    PrettyTree::Annotated { color, doc } => {
                        out.push_annotation(color.clone())?;
                        self.annotation_levels.push(self.back_cmds.len());
                        cmd.node = doc.clone();
                        continue;
                    }
                    PrettyTree::Union { left, right } => {
                        let pos = self.pos;
                        let annotation_levels = self.annotation_levels.len();
                        let bcmds = self.back_cmds.len();

                        self.back_cmds.push(RenderCommand { indent: ind, mode, node: left.clone() });

                        let mut buffer = BufferWrite::new();

                        match self.best(bcmds, &mut buffer) {
                            Ok(true) => buffer.render(out)?,
                            Ok(false) | Err(()) => {
                                self.pos = pos;
                                self.back_cmds.truncate(bcmds);
                                self.annotation_levels.truncate(annotation_levels);
                                cmd.node = right.clone();
                                continue;
                            }
                        }
                    }
                    PrettyTree::Column { function: column } => {
                        cmd.node = Rc::new(column(self.pos));
                        continue;
                    }
                    PrettyTree::Nesting { function: nesting } => {
                        cmd.node = Rc::new(nesting(self.pos));
                        continue;
                    }
                    PrettyTree::Fail => return Err(out.fail_doc()),
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
