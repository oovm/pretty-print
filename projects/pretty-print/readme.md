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

