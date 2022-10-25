#![allow(dead_code, unused)]
use pretty_print::{AnsiColor, AnsiStyle, DocumentTree};
use std::{io::stdout, rc::Rc};

#[test]
fn ready() {
    println!("it works!")
}

#[test]
fn box1() {
    let doc = DocumentTree::text("hello")
        .annotate(Rc::new(AnsiStyle::new(AnsiColor::Red)))
        .append(DocumentTree::Hardline)
        .append("a")
        .append(" ")
        .append("world");
    let mut buffer = vec![];
    doc.render_colored(10, &mut buffer).unwrap();

    println!("{}", String::from_utf8_lossy(&buffer));
}

#[test]
fn box2() {
    let doc = DocumentTree::concat(vec!["hello", " ", " ", "world"]).group();
    println!("{}", doc.pretty(10));
}

#[test]
fn box_doc_inference() {
    let doc = DocumentTree::text("test").append(DocumentTree::line()).append(DocumentTree::text("test")).group();

    println!("{}", doc.pretty(10));
}

#[test]
fn newline_in_text() {
    let doc = DocumentTree::text("test")
        .append(DocumentTree::line().append(DocumentTree::text("\"test\n     test\"")).nest(4))
        .group();
    println!("{}", doc.pretty(10));
}
