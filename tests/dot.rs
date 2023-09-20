use colored::Colorize as _;
use include_dir::{include_dir, Dir};
use miette::{Diagnostic, NamedSource, SourceOffset, SourceSpan};
use thiserror::Error;

const GRAPHVIZ_GALLERY: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/test-vectors/graphviz-gallery");

#[test]
fn parse_gallery() {
    let mut fail = vec![];
    let mut pass = 0;
    for gallery_item in GRAPHVIZ_GALLERY.files() {
        let path = gallery_item.path().display();
        print!("vector {path}...");
        let contents = gallery_item.contents_utf8().unwrap();
        let removed_comments = contents
            .lines()
            .filter(|it| !it.starts_with('#'))
            .collect::<Vec<_>>()
            .as_slice()
            .join("\n");
        match syn::parse_str::<syn_graphs::dot::Graph>(removed_comments.as_str()) {
            Ok(_) => {
                pass += 1;
                println!("{}", "ok.".green())
            }
            Err(syn) => {
                println!("{}", "FAIL".red());

                let start = syn.span().start();
                let end = syn.span().end();
                let e = TestCaseFailure {
                    span: SourceSpan::new(
                        SourceOffset::from_location(&removed_comments, start.line, start.column),
                        SourceOffset::from_location(&removed_comments, end.line, end.column),
                    ),
                    src: NamedSource::new(path.to_string(), removed_comments),
                    syn,
                };
                fail.push(e);
            }
        }
    }

    match fail.len() {
        0 => println!("{pass} vectors succeeded."),
        nonzero => {
            for e in fail {
                let mut out = String::new();
                match miette::GraphicalReportHandler::new().render_report(&mut out, &e) {
                    Ok(()) => println!("{out}"),
                    Err(_) => println!("<FORMATTER FAILURE>\n{}\n", e.syn),
                };
            }
            panic!("{nonzero} vectors failed ({pass} succeeded).")
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("test case failed")]
pub struct TestCaseFailure {
    #[source_code]
    src: NamedSource,
    #[source]
    syn: syn::Error,
    #[label]
    span: SourceSpan,
}
