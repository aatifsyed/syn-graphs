use std::{
    fmt::{self, Display},
    str::FromStr as _,
};

use colored::Colorize as _;
use either::Either;
use include_dir::{include_dir, Dir};
use miette::{Diagnostic, NamedSource, SourceOffset, SourceSpan};
use pretty_assertions::Comparison;
use proc_macro2::{LexError, TokenStream};
use syn_graphs::{dot::Graph, unparse};
use thiserror::Error;

const GRAPHVIZ_GALLERY: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/test-vectors/graphviz-gallery");

fn parse(source_name: impl Display, source: &str) -> Result<Graph, Either<LexError, ParseError>> {
    let removed_comments = source
        .lines()
        .filter(|it| !it.starts_with('#'))
        .collect::<Vec<_>>()
        .as_slice()
        .join("\n");
    match TokenStream::from_str(removed_comments.as_str()) {
        Ok(token_stream) => match syn::parse2(token_stream) {
            Ok(graph) => Ok(graph),
            Err(syn_error) => Err(Either::Right(ParseError::new(
                syn_error,
                source_name,
                removed_comments,
            ))),
        },
        Err(lex_error) => Err(Either::Left(lex_error)),
    }
}

#[test]
fn gallery() {
    let mut fail = vec![];
    let mut skipped = 0;
    let mut pass = 0;
    for gallery_item in GRAPHVIZ_GALLERY.files() {
        let path = gallery_item.path();
        print!("vector {}...", path.display());
        match parse(
            path.display(),
            gallery_item
                .contents_utf8()
                .expect("test cases are all utf-8"),
        ) {
            Ok(mut first) => {
                let source2 = unparse::dot(&first);
                match parse(format!("{} (round trip)", path.display()), source2.as_str()) {
                    Ok(mut second) => {
                        fixup::visit_graph(&mut first);
                        fixup::visit_graph(&mut second);
                        match first == second {
                            true => {
                                println!("{}", "ok".green());
                                pass += 1;
                            }
                            false => {
                                println!("{} (mismatch)", "FAIL".red());
                                fail.push(TestCaseFailure::Mismatch(first, second))
                            }
                        }
                    }
                    Err(Either::Left(lex_error)) => {
                        println!("{} (second lex)", "FAIL".red());
                        fail.push(TestCaseFailure::SecondLex(lex_error))
                    }
                    Err(Either::Right(parse_error)) => {
                        println!("{} (second parse)", "FAIL".red());
                        fail.push(TestCaseFailure::SecondParse(parse_error))
                    }
                }
            }
            Err(Either::Left(_lex_error)) => {
                println!("{} (lex error)", "skipped".yellow());
                skipped += 1;
            }
            Err(Either::Right(parse_error)) => {
                println!("{} (first parse)", "FAIL".red());
                fail.push(TestCaseFailure::FirstParse(parse_error));
            }
        }
    }

    match fail.len() {
        0 => println!("{pass} vectors succeeded ({skipped} skipped)."),
        nonzero => {
            for fail in fail {
                println!("{}", fail);
            }
            panic!("{nonzero} vectors failed ({pass} succeeded, {skipped} skipped).")
        }
    }
}

enum TestCaseFailure {
    FirstParse(ParseError),
    SecondParse(ParseError),
    SecondLex(LexError),
    Mismatch(Graph, Graph),
}

impl fmt::Display for TestCaseFailure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TestCaseFailure::FirstParse(it) | TestCaseFailure::SecondParse(it) => {
                f.write_str(&it.pretty())
            }
            TestCaseFailure::SecondLex(it) => f.write_fmt(format_args!("{}", it)),
            TestCaseFailure::Mismatch(left, right) => {
                f.write_fmt(format_args!("{}", Comparison::new(left, right)))
            }
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("test case failed")]
pub struct ParseError {
    #[source_code]
    src: NamedSource,
    #[source]
    syn: syn::Error,
    #[label]
    span: SourceSpan,
}

impl ParseError {
    pub fn new(syn: syn::Error, source_name: impl Display, source_text: String) -> Self {
        let start = syn.span().start();
        let end = syn.span().end();
        Self {
            span: SourceSpan::new(
                SourceOffset::from_location(&source_text, start.line, start.column),
                SourceOffset::from_location(&source_text, end.line, end.column),
            ),
            src: NamedSource::new(source_name.to_string(), source_text),
            syn,
        }
    }
    pub fn pretty(&self) -> String {
        let renderer = miette::GraphicalReportHandler::new();
        let mut out = String::new();
        match renderer.render_report(&mut out, self) {
            Ok(()) => out,
            Err(render_error) => format!("{} (could not render: {})", self.syn, render_error),
        }
    }
}

/// Remove semantically irrelevant information
mod fixup {
    use proc_macro2::{Span, TokenStream};
    use syn::token;
    use syn_graphs::dot::{
        kw, AttrSep, Attributes, Graph, NodeIdOrSubgraph, Statements, Stmt, StmtSubgraph, ID,
    };
    pub fn visit_graph(it: &mut Graph) {
        visit_statements(&mut it.statements)
    }

    fn visit_statements(it: &mut Statements) {
        for (it, punct) in &mut it.list {
            visit_stmt(it);
            *punct = Some(token::Semi::default());
        }
    }

    fn visit_stmt(it: &mut Stmt) {
        match it {
            Stmt::Attr(it) => visit_attributes(&mut it.attributes),
            Stmt::Assign(it) => {
                visit_id(&mut it.left);
                visit_id(&mut it.right);
            }
            Stmt::Node(it) => {
                visit_id(&mut it.node_id.id);
                if let Some(it) = it.attributes.as_mut() {
                    visit_attributes(it)
                }
            }
            Stmt::Edge(it) => {
                visit_node_id_or_subgraph(&mut it.from);
                for (_, it) in &mut it.ops {
                    visit_node_id_or_subgraph(it)
                }
                if let Some(it) = it.attrs.as_mut() {
                    visit_attributes(it)
                }
            }
            Stmt::Subgraph(it) => visit_subgraph(it),
        }
    }

    fn visit_node_id_or_subgraph(it: &mut NodeIdOrSubgraph) {
        match it {
            NodeIdOrSubgraph::Subgraph(it) => visit_subgraph(it),
            NodeIdOrSubgraph::NodeId(it) => visit_id(&mut it.id),
        }
    }

    fn visit_subgraph(it: &mut StmtSubgraph) {
        match &mut it.prelude {
            Some((_, Some(it))) => visit_id(it),
            Some((_, None)) => {}
            None => it.prelude = Some((kw::subgraph::default(), None)),
        }
        visit_statements(&mut it.statements)
    }

    fn visit_attributes(it: &mut Attributes) {
        for it in it.lists.iter_mut().flat_map(|it| it.kvs.iter_mut()) {
            visit_id(&mut it.left);
            visit_id(&mut it.right);
            it.trailing = Some(AttrSep::Comma(token::Comma::default()));
        }
    }

    fn visit_id(it: &mut ID) {
        match it {
            ID::AnyIdent(it) => it.set_span(Span::call_site()),
            ID::AnyLit(_) => {}
            ID::Html(it) => {
                it.stream = TokenStream::new() // can't sanitise the spans :(
            }
            ID::DotInt(_) => {}
        }
    }
}
