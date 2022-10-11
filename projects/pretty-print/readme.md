This crate defines a
[Wadler-style](http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf)
pretty-printing API.

Start with the static functions of [Doc](enum.Doc.html).

## Quick start

Let's pretty-print simple sexps!  We want to pretty print sexps like

```lisp
(1 2 3)
```
or, if the line would be too long, like

```lisp
((1)
 (2 3)
 (4 5 6))
```

A _simple symbolic expression_ consists of a numeric _atom_ or a nested ordered _list_ of
symbolic expression children.

```rust
# use pretty::*;
enum SExp {
    Atom(u32),
    List(Vec<SExp>),
}
use SExp::*;
# fn main() { }
```

We define a simple conversion to a [Doc](enum.Doc.html).  Atoms are rendered as strings; lists
are recursively rendered, with spaces between children where appropriate.  Children are
[nested]() and [grouped](), allowing them to be laid out in a single line as appropriate.

```rust
# use pretty::*;
# enum SExp {
#     Atom(u32),
#     List(Vec<SExp>),
# }
# use SExp::*;
impl SExp {
    /// Return a pretty printed format of self.
    pub fn to_doc(&self) -> RcDoc<()> {
        match *self {
            Atom(ref x) => RcDoc::as_string(x),
            List(ref xs) =>
                RcDoc::text("(")
                    .append(RcDoc::intersperse(xs.into_iter().map(|x| x.to_doc()), Doc::line()).nest(1).group())
                    .append(RcDoc::text(")"))
        }
    }
}
# fn main() { }
```

Next, we convert the [Doc](enum.Doc.html) to a plain old string.

```rust
# use pretty::*;
# enum SExp {
#     Atom(u32),
#     List(Vec<SExp>),
# }
# use SExp::*;
# impl SExp {
#     /// Return a pretty printed format of self.
#     pub fn to_doc(&self) -> BoxDoc<()> {
#         match *self {
#             Atom(ref x) => BoxDoc::as_string(x),
#             List(ref xs) =>
#                 BoxDoc::text("(")
#                     .append(BoxDoc::intersperse(xs.into_iter().map(|x| x.to_doc()), Doc::line()).nest(1).group())
#                     .append(BoxDoc::text(")"))
#         }
#     }
# }
impl SExp {
    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}
# fn main() { }
```

And finally we can test that the nesting and grouping behaves as we expected.

```rust
# use pretty::*;
# enum SExp {
#     Atom(u32),
#     List(Vec<SExp>),
# }
# use SExp::*;
# impl SExp {
#     /// Return a pretty printed format of self.
#     pub fn to_doc(&self) -> BoxDoc<()> {
#         match *self {
#             Atom(ref x) => BoxDoc::as_string(x),
#             List(ref xs) =>
#                 BoxDoc::text("(")
#                     .append(BoxDoc::intersperse(xs.into_iter().map(|x| x.to_doc()), Doc::line()).nest(1).group())
#                     .append(BoxDoc::text(")"))
#         }
#     }
# }
# impl SExp {
#     pub fn to_pretty(&self, width: usize) -> String {
#         let mut w = Vec::new();
#         self.to_doc().render(width, &mut w).unwrap();
#         String::from_utf8(w).unwrap()
#     }
# }
# fn main() {
let atom = SExp::Atom(5);
assert_eq!("5", atom.to_pretty(10));
let list = SExp::List(vec![SExp::Atom(1), SExp::Atom(2), SExp::Atom(3)]);
assert_eq!("(1 2 3)", list.to_pretty(10));
assert_eq!("\
(1
 2
 3)", list.to_pretty(5));
# }
```

## Advanced usage

There's a more efficient pattern that uses the [DocAllocator](trait.DocAllocator.html) trait, as
implemented by [BoxAllocator](struct.BoxAllocator.html), to allocate
[DocBuilder](struct.DocBuilder.html) instances.  See
[examples/trees.rs](https://github.com/freebroccolo/pretty.rs/blob/master/examples/trees.rs#L39)
for this approach.