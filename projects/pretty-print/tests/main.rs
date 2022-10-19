#![allow(dead_code, unused)]
use std::io::stdout;
use termcolor::{Buffer, Color, ColorChoice, ColorSpec, StandardStream};
use pretty_print::DocumentTree;

#[test]
fn ready() {
    println!("it works!")
}


#[test]
fn box1() {
    let doc = DocumentTree::text("hello").annotate(ColorSpec::new().set_fg(Some(Color::Red)).clone()).append(DocumentTree::Hardline).append(" ").append("world");
    let mut buffer = Buffer::ansi();
    doc.render_colored(10, &mut buffer).unwrap();

    println!("{}", String::from_utf8_lossy(&buffer.into_inner()));
}


#[test]
fn box2() {
    let doc = DocumentTree::concat(vec!["hello", " ", " ", "world"]);

    println!("{}", doc.pretty(10));
}

#[test]
fn box_doc_inference() {
    let doc = DocumentTree::text("test")
        .append(DocumentTree::line())
        .append(DocumentTree::text("test"))
        .group();

    println!("{}", doc.pretty(10));
}

#[test]
fn newline_in_text() {
    let doc = DocumentTree::text("test").append(
        DocumentTree::line()
            .append(DocumentTree::text("\"test\n     test\""))
            .nest(4),
    ).group();
    println!("{}", doc.pretty(10));
}