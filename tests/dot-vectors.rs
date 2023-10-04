use std::{ffi::OsStr, fmt::Display, str::FromStr as _};

use colored::Colorize as _;
use include_dir::{include_dir, Dir};
use pretty_assertions::Comparison;
use proc_macro2::TokenStream;
use syn_graphs::{dot::Graph, unparse};

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
                        println!("{}", render(&e));
                    }
                }
            }
            Err(e) => {
                println!("{} (first parse)", "FAIL".red());
                println!("{}", render(&e));
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

fn parse(file_name: impl Display, source: &str) -> Result<Graph, syn_miette::Error> {
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
            Err(syn_error) => Err(syn_miette::Error::new_named(syn_error, source, file_name)),
        },
        Err(lex_error) => Err(syn_miette::Error::new_named(
            syn::Error::from(lex_error),
            source,
            file_name,
        )),
    }
}

fn render(error: &syn_miette::Error) -> String {
    let renderer = miette::GraphicalReportHandler::new();
    let mut out = String::new();
    renderer.render_report(&mut out, error).unwrap();
    out
}

/// Remove semantically irrelevant information
mod fixup {
    use proc_macro2::{Span, TokenStream};
    use syn::token;
    use syn_graphs::dot::{
        kw, AttrSep, Attrs, EdgeTarget, Graph, Stmt, StmtList, StmtSubgraph, ID,
    };
    pub fn visit_graph(it: &mut Graph) {
        visit_stmt_list(&mut it.stmt_list)
    }

    fn visit_stmt_list(it: &mut StmtList) {
        for (stmt, semi) in &mut it.stmts {
            visit_stmt(stmt);
            *semi = Some(token::Semi::default());
        }
    }

    fn visit_stmt(it: &mut Stmt) {
        match it {
            Stmt::Attr(it) => visit_attributes(&mut it.attrs),
            Stmt::Assign(it) => {
                visit_id(&mut it.left);
                visit_id(&mut it.right);
            }
            Stmt::Node(it) => {
                visit_id(&mut it.node_id.id);
                if let Some(it) = it.attrs.as_mut() {
                    visit_attributes(it)
                }
            }
            Stmt::Edge(it) => {
                visit_node_id_or_subgraph(&mut it.from);
                for (_, it) in &mut it.edges {
                    visit_node_id_or_subgraph(it)
                }
                if let Some(it) = it.attrs.as_mut() {
                    visit_attributes(it)
                }
            }
            Stmt::Subgraph(it) => visit_subgraph(it),
        }
    }

    fn visit_node_id_or_subgraph(it: &mut EdgeTarget) {
        match it {
            EdgeTarget::Subgraph(it) => visit_subgraph(it),
            EdgeTarget::NodeId(it) => visit_id(&mut it.id),
        }
    }

    fn visit_subgraph(it: &mut StmtSubgraph) {
        match &mut it.prelude {
            Some((_, Some(it))) => visit_id(it),
            Some((_, None)) => {}
            None => it.prelude = Some((kw::subgraph::default(), None)),
        }
        visit_stmt_list(&mut it.statements)
    }

    fn visit_attributes(it: &mut Attrs) {
        for it in it.lists.iter_mut().flat_map(|it| it.assigns.iter_mut()) {
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
