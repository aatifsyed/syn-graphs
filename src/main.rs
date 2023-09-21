use std::{
    collections::VecDeque,
    error::Error,
    fmt::{Display, Write},
    fs::File,
    io::Read,
    str::FromStr,
};

use proc_macro2::TokenStream;
use syn::parse::{Parse as _, Parser as _};
use syn_graphs::dot::Graph;

use Print::{Atom, Compound};
use PrintAtom::{Dedent, Indent, Line};

#[derive(Debug)]
enum PrintAtom {
    Indent,
    Dedent,
    Line(String),
}
#[derive(Debug)]
enum Print {
    Atom(PrintAtom),
    Compound(Vec<Self>),
}

impl Print {
    fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = &'a PrintAtom> + 'a> {
        match &self {
            Self::Atom(atom) => Box::new(std::iter::once(atom)),
            Self::Compound(c) => Box::new(c.iter().flat_map(|it| it.iter())),
        }
    }
}

fn line<const N: usize>(items: [&dyn Display; N]) -> PrintAtom {
    let mut s = String::new();
    for item in items {
        s.write_fmt(format_args!("{}", item)).unwrap()
    }
    PrintAtom::Line(s)
}
fn just(s: &str) -> PrintAtom {
    PrintAtom::Line(String::from(s))
}

fn main() -> Result<(), Box<dyn Error>> {
    // let mut s = String::new();
    // File::open("/dev/stdin")?.read_to_string(&mut s)?;
    // let ts = TokenStream::from_str(&s)?;
    // let graph = syn_graphs::dot::Graph::parse.parse2(ts)?;
    let print = Compound(vec![
        Atom(just("digraph example {")),
        Compound(vec![
            Atom(Indent),
            Atom(just("alice -> bob;")),
            Atom(just("charlie -> subgraph {")),
            Compound(vec![
                Atom(Indent),
                Atom(just("gabrielle -> jonah;")),
                Atom(Dedent),
            ]),
            Atom(just("}")),
            Atom(Dedent),
        ]),
        Atom(just("}")),
    ]);
    let mut depth = 0;
    for atom in print.iter() {
        match atom {
            Indent => depth += 1,
            Dedent => depth -= 1,
            Line(s) => {
                for _ in 0..depth {
                    print!("  ")
                }
                println!("{}", s)
            }
        }
    }

    Ok(())
}

fn graph(
    Graph {
        strict,
        direction,
        id,
        brace_token,
        statements,
    }: &Graph,
) -> Print {
    let mut items = vec![];
    if strict.is_some() {
        items.push(value)
    }
    todo!()
}
