use std::{error::Error, ffi::OsStr, fmt::Display, str::FromStr as _};

use colored::Colorize as _;
use either::Either;
use include_dir::{include_dir, Dir};
use miette::{Diagnostic, NamedSource, SourceOffset, SourceSpan};
use pretty_assertions::Comparison;
use proc_macro2::{Span, TokenStream};
use syn_graphs::{dot::Graph, unparse};
use thiserror::Error;

const GRAPHVIZ_GALLERY: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/test-vectors/dot");
const SKIP: &[&str] = &[
    "base.gv",         // syntax error online too
    "b545.gv",         // syntax error online too
    "b993.gv",         // uses `diGraph` instead of `digraph`
    "multi.gv",        // don't support multiple graphs in one document
    "polypoly.gv.~1~", // BUG?(aatifsyed)
    "polypoly.gv.~2~", // BUG?(aatifsyed)
    "world.gv.~2~",    // BUG?(aatifsyed)
];

#[test]
fn vectors() {
    let mut fail = 0;
    let mut skipped = 0;
    let mut pass = 0;
    for gallery_item in GRAPHVIZ_GALLERY.files() {
        let path = gallery_item.path();
        print!("vector {}...", path.display());
        let Some(source) = gallery_item.contents_utf8() else {
            println!("{} (non-utf-8)", "skipped".yellow());
            skipped += 1;
            continue;
        };
        if path
            .file_name()
            .is_some_and(|it| SKIP.iter().any(|skip| OsStr::new(skip) == it))
        {
            println!("{} (listed)", "skipped".yellow());
            skipped += 1;
            continue;
        }
        match parse(path.display(), source) {
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
                                continue;
                            }
                            false => {
                                println!("{} (mismatch)", "FAIL".red());
                                println!("{}", Comparison::new(&first, &second));
                            }
                        }
                    }
                    Err(e) => {
                        println!("{} (second parse)", "FAIL".red());
                        println!("{}", e.pretty());
                    }
                }
            }
            Err(e) => {
                println!("{} (first parse)", "FAIL".red());
                println!("{}", e.pretty());
            }
        }
        fail += 1;
    }

    match fail {
        0 => println!("{pass} vectors succeeded ({skipped} skipped)."),
        nonzero => {
            panic!("{nonzero} vectors failed ({pass} succeeded, {skipped} skipped).")
        }
    }
}

fn parse(
    source_name: impl Display,
    source: &str,
) -> Result<Graph, SpannedError<Either<proc_macro2::LexError, syn::Error>>> {
    let source = source
        .replace("\\N", "N")
        .replace("\\G", "G")
        .replace("\\E", "E")
        .replace("\\<", "<")
        .replace("\\>", ">")
        .replace("\\l", "l")
        .replace('«', "<<")
        .replace('»', ">>");
    let source = source // remove comments
        .lines()
        .filter(|it| !it.starts_with('#'))
        .collect::<Vec<_>>()
        .as_slice()
        .join("\n");
    match TokenStream::from_str(source.as_str()) {
        Ok(token_stream) => match syn::parse2(token_stream) {
            Ok(graph) => Ok(graph),
            Err(syn_error) => Err(SpannedError::new(
                syn_error.span(),
                Either::Right(syn_error),
                source_name,
                source,
            )),
        },
        Err(lex_error) => Err(SpannedError::new(
            lex_error.span(),
            Either::Left(lex_error),
            source_name,
            source,
        )),
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("{}", inner)]
pub struct SpannedError<E: Error + 'static> {
    #[source_code]
    src: NamedSource,
    #[source]
    inner: E,
    #[label]
    span: SourceSpan,
}
impl<E: Error + 'static> SpannedError<E> {
    pub fn new(span: Span, inner: E, source_name: impl Display, source_text: String) -> Self {
        let start = span.start();
        let end = span.end();
        Self {
            span: SourceSpan::new(
                SourceOffset::from_location(&source_text, start.line, start.column),
                SourceOffset::from_location(&source_text, end.line, end.column),
            ),
            src: NamedSource::new(source_name.to_string(), source_text),
            inner,
        }
    }

    pub fn pretty(&self) -> String {
        let renderer = miette::GraphicalReportHandler::new();
        let mut out = String::new();
        match renderer.render_report(&mut out, self) {
            Ok(()) => {
                if out.lines().count() > 50 {
                    out = out.lines().take(49).collect::<Vec<_>>().join("\n");
                    out.push_str("\n<truncated>")
                }
                out
            }
            Err(render_error) => format!(
                "{} at offset {} (could not render: {})",
                self.inner,
                self.span.offset(),
                render_error
            ),
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
