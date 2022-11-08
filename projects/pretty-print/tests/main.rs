#[test]
fn ready() {
    println!("it works!")
}

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
}
