# Pretty Printer

This crate defines a
[Wadler-style](http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf)
pretty-printing API.

Start with the static functions of [PrettyTree](enum.Doc.html).

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
use pretty_print::*;
use SExp::*;
enum SExp {
    Atom(u32),
    List(Vec<SExp>),
}

impl SExp {
    /// Return a pretty printed format of self.
    pub fn to_doc(&self) -> PrettyTree {
        match self {
            Atom(x) => PrettyTree::text(x.to_string()),
            List(xs) => PrettyTree::text("(")
                .append(PrettyTree::join(xs.into_iter().map(|x| x.to_doc()), PrettyTree::line_or_space()).nest(1).group())
                .append(PrettyTree::text(")")),
        }
    }
    /// Return a pretty printed format of self.
    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

fn main() {
    let atom = Atom(5);
    assert_eq!("5", atom.to_pretty(10));
    let list = List(vec![Atom(1), Atom(2), Atom(3)]);
    assert_eq!("(1 2 3)", list.to_pretty(10));
    assert_eq!(
        "\
(1
 2
 3)",
        list.to_pretty(5)
    );
}
```