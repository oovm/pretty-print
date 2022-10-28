#![allow(dead_code, unused)]
use pretty_print::{AnsiColor, AnsiStyle, PrettyTree};
use std::{io::stdout, rc::Rc};

#[test]
fn ready() {
    println!("it works!")
}

#[test]
fn box1() {
    let doc = PrettyTree::text("hello")
        .annotate(Rc::new(AnsiStyle::new(AnsiColor::Red)))
        .append(PrettyTree::Hardline)
        .append("a")
        .append(" ")
        .append("world");
    let mut buffer = vec![];
    doc.render_colored(10, &mut buffer).unwrap();

    println!("{}", String::from_utf8_lossy(&buffer));
}

#[test]
fn box2() {
    let doc = PrettyTree::concat(vec!["hello", " ", " ", "world"]).group();
    println!("{}", doc.pretty(10));
}

#[test]
fn box_doc_inference() {
    let doc = PrettyTree::text("test").append(PrettyTree::line()).append(PrettyTree::text("test")).group();

    println!("{}", doc.pretty(10));
}

#[test]
fn newline_in_text() {
    let doc =
        PrettyTree::text("test").append(PrettyTree::line().append(PrettyTree::text("\"test\n     test\"")).nest(4)).group();
    println!("{}", doc.pretty(10));
}
