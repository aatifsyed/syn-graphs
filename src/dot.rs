use crate::{enum_of_kws, CodeFormatterExt as _, Printable};
use derive_quote_to_tokens::ToTokens;
use derive_syn_parse::Parse;
use indenter::CodeFormatter;
#[cfg(test)]
use pretty_assertions::assert_eq;
use proc_macro2::TokenStream;
use quote::ToTokens;
use std::fmt::{self, Write as _};
use syn::{
    ext::IdentExt as _,
    parse::{Parse, ParseStream},
    token, Token,
};

pub mod kw {
    use super::*;
    macro_rules! custom_keywords {
        ($($it:ident),* $(,)?) => {
            $(
                syn::custom_keyword!($it);
                impl Printable for $it {
                    fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result {
                        f.write_str(stringify!($it))
                    }
                }
            )*
        };
    }
    custom_keywords!(strict, graph, digraph, node, edge, subgraph, n, ne, e, se, s, sw, w, nw, c,);
}

pub mod pun {
    use super::*;

    syn::custom_punctuation!(DirectedEdge, ->);
    impl Printable for DirectedEdge {
        fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result {
            f.write_str("->")
        }
    }
}

#[derive(Parse)]
pub struct Graph {
    pub strict: Option<kw::strict>,
    pub direction: Directedness,
    #[call(Self::maybe_id)]
    pub id: Option<ID>,
    #[brace]
    pub brace_token: token::Brace,
    #[inside(brace_token)]
    pub statements: Statements,
}

impl Graph {
    fn maybe_id(input: ParseStream) -> syn::Result<Option<ID>> {
        match input.peek(token::Brace) {
            true => Ok(None),
            false => Ok(Some(input.parse()?)),
        }
    }
}

impl ToTokens for Graph {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            strict,
            direction,
            id,
            brace_token,
            statements,
        } = self;
        strict.to_tokens(tokens);
        direction.to_tokens(tokens);
        id.to_tokens(tokens);
        brace_token.surround(tokens, |tokens| statements.to_tokens(tokens))
    }
}

impl Printable for Graph {
    fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result {
        let Self {
            strict,
            direction,
            id,
            brace_token,
            statements,
        } = self;
        if let Some(strict) = strict {
            f.print(strict)?;
            f.sep()?
        }
        f.print(direction)?;
        f.sep()?;
        if let Some(id) = id {
            f.print(id)?;
            f.sep()?
        }
        f.delimit(brace_token, |f| {
            f.newline()?;
            f.print(statements)
        })
    }
}

impl fmt::Display for Graph {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut f = CodeFormatter::new(f, "    ");
        f.print(self)
    }
}

enum_of_kws!(
    pub enum Directedness {
        #[name = "graph"]
        Graph(kw::graph),
        #[name = "digraph"]
        Digraph(kw::digraph),
    }
);

#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub struct Statements {
    pub list: Vec<(Stmt, Option<Token![;]>)>,
}

impl Parse for Statements {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut list = vec![];
        while !input.is_empty() {
            let stmt = input.parse()?;
            let semi = match input.peek(Token![;]) {
                true => Some(input.parse()?),
                false => None,
            };
            list.push((stmt, semi))
        }
        Ok(Self { list })
    }
}

impl ToTokens for Statements {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { list } = self;
        for (stmt, semi) in list {
            stmt.to_tokens(tokens);
            semi.to_tokens(tokens)
        }
    }
}

impl Printable for Statements {
    fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result {
        let Self { list } = self;
        for (stmt, _always_semi) in list {
            f.print(stmt)?;
            f.print(token::Semi::default())?;
            f.newline()?;
        }
        Ok(())
    }
}

#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
#[derive(ToTokens)]
pub enum Stmt {
    Attr(StmtAttr),
    Assign(StmtAssign),
    Node(StmtNode),
    Edge(StmtEdge),
    Subgraph(StmtSubgraph),
}

impl Parse for Stmt {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.fork().parse::<StmtAttrOn>().is_ok() {
            return Ok(Self::Attr(input.parse()?));
        }
        if input.peek2(Token![=]) {
            return Ok(Self::Assign(input.parse()?));
        }
        if input.fork().parse::<StmtEdge>().is_ok() {
            return Ok(Self::Edge(input.parse()?));
        }
        if input.peek(kw::subgraph) || input.peek(token::Brace) {
            return Ok(Self::Subgraph(input.parse()?));
        }
        Ok(Self::Node(input.parse()?))
    }
}

impl Printable for Stmt {
    fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result {
        match self {
            Stmt::Attr(it) => f.print(it),
            Stmt::Assign(it) => f.print(it),
            Stmt::Node(it) => f.print(it),
            Stmt::Edge(it) => f.print(it),
            Stmt::Subgraph(it) => f.print(it),
        }
    }
}

#[test]
fn parse_stmt() {
    assert_eq!(
        Stmt::Edge(StmtEdge {
            from: NodeIdOrSubgraph::NodeId(NodeId {
                id: ID::lit_str("node0"),
                port: Some(Port::ID {
                    colon: tok::colon(),
                    id: ID::ident("f0")
                })
            }),
            ops: vec![(
                EdgeOp::directed(),
                NodeIdOrSubgraph::NodeId(NodeId {
                    id: ID::lit_str("node1"),
                    port: Some(Port::ID {
                        colon: tok::colon(),
                        id: ID::ident("f0")
                    })
                })
            )],
            attrs: None
        }),
        syn::parse_quote!("node0":f0 -> "node1":f0)
    )
}
#[derive(ToTokens, Parse)]
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub struct StmtAssign {
    pub left: ID,
    pub eq_token: Token![=],
    pub right: ID,
}

impl Printable for StmtAssign {
    fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result {
        let Self {
            left,
            eq_token,
            right,
        } = self;
        f.print(left)?;
        f.sep()?;
        f.print(eq_token)?;
        f.sep()?;
        f.print(right)
    }
}

#[derive(Parse, ToTokens)]
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub struct StmtAttr {
    pub on: StmtAttrOn,
    pub attributes: Attributes,
}

impl Printable for StmtAttr {
    fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result {
        let Self { on, attributes } = self;
        f.print(on)?;
        f.sep()?;
        f.print(attributes)
    }
}

enum_of_kws!(
    pub enum StmtAttrOn {
        #[name = "graph"]
        Graph(kw::graph),
        #[name = "node"]
        Node(kw::node),
        #[name = "edge"]
        Edge(kw::edge),
    }
);

#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub struct Attributes {
    /// Non-empty
    pub lists: Vec<AttrList>,
}

impl Parse for Attributes {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut lists = vec![input.parse()?];
        while input.peek(token::Bracket) {
            lists.push(input.parse()?)
        }
        Ok(Self { lists })
    }
}

impl ToTokens for Attributes {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { lists } = self;
        for list in lists {
            list.to_tokens(tokens)
        }
    }
}

impl Printable for Attributes {
    fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result {
        let Self { lists } = self;
        for lis in lists {
            lis.print_to(f)?
        }
        Ok(())
    }
}

#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub struct AttrList {
    pub bracket_token: token::Bracket,
    pub kvs: Vec<AttrKV>,
}

impl Parse for AttrList {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut kvs = vec![];
        let content;
        let bracket_token = syn::bracketed!(content in input);
        while !content.is_empty() {
            kvs.push(content.parse()?)
        }
        Ok(Self { bracket_token, kvs })
    }
}

impl ToTokens for AttrList {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { bracket_token, kvs } = self;
        bracket_token.surround(tokens, |inner| {
            for kv in kvs {
                kv.to_tokens(inner)
            }
        })
    }
}

impl Printable for AttrList {
    fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result {
        let Self { bracket_token, kvs } = self;
        f.delimit(bracket_token, |f| {
            let do_nl = kvs.len() > 1;
            for kv in kvs {
                f.print(kv)?;
                if do_nl {
                    f.newline()?
                }
            }
            Ok(())
        })
    }
}

#[derive(ToTokens, Parse)]
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub struct AttrKV {
    pub left: ID,
    pub eq_token: Token![=],
    pub right: ID,
    #[call(Self::parse_sep)]
    pub trailing: Option<AttrSep>,
}

impl Printable for AttrKV {
    fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result {
        let Self {
            left,
            eq_token,
            right,
            trailing: _always_semi,
        } = self;
        f.print(left)?;
        f.sep()?;
        f.print(eq_token)?;
        f.sep()?;
        f.print(right)?;
        f.print(token::Semi::default())
    }
}

#[cfg(test)]
impl AttrKV {
    fn ident_and_string(ident: &str, string: &str) -> Self {
        Self {
            left: ID::ident(ident),
            eq_token: tok::eq(),
            right: ID::lit_str(string),
            trailing: None,
        }
    }
}

impl AttrKV {
    fn parse_sep(input: ParseStream) -> syn::Result<Option<AttrSep>> {
        if input.peek(Token![,]) || input.peek(Token![;]) {
            return Ok(Some(input.parse()?));
        }
        Ok(None)
    }
}

enum_of_kws!(
    pub enum AttrSep {
        #[name = ","]
        Comma(token::Comma),
        #[name = ";"]
        Semi(token::Semi),
    }
);

#[derive(Parse)]
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub struct StmtEdge {
    pub from: NodeIdOrSubgraph,
    /// Non-empty
    #[call(Self::parse_ops)]
    pub ops: Vec<(EdgeOp, NodeIdOrSubgraph)>,
    #[peek(token::Bracket)]
    pub attrs: Option<Attributes>,
}

impl StmtEdge {
    fn parse_ops(input: ParseStream) -> syn::Result<Vec<(EdgeOp, NodeIdOrSubgraph)>> {
        let mut ops = vec![(input.parse()?, input.parse()?)];
        while input.peek(pun::DirectedEdge) || input.peek(Token![-]) {
            ops.push((input.parse()?, input.parse()?))
        }
        Ok(ops)
    }
}

impl Printable for StmtEdge {
    fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result {
        let Self { from, ops, attrs } = self;
        f.print(from)?;
        for (op, to) in ops {
            f.sep()?;
            f.print(op)?;
            f.sep()?;
            f.print(to)?
        }
        if let Some(attrs) = attrs {
            f.sep()?;
            f.print(attrs)?
        }
        Ok(())
    }
}

#[test]
fn parse_stmt_edge() {
    assert_eq!(
        StmtEdge {
            from: NodeIdOrSubgraph::ident("alice"),
            ops: vec![(EdgeOp::undirected(), NodeIdOrSubgraph::ident("bob"))],
            attrs: None
        },
        syn::parse_quote! {
            alice -- bob
        }
    )
}

impl ToTokens for StmtEdge {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { from, ops, attrs } = self;
        from.to_tokens(tokens);
        for (op, to) in ops {
            op.to_tokens(tokens);
            to.to_tokens(tokens)
        }
        attrs.to_tokens(tokens)
    }
}

#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
#[derive(ToTokens)]
pub enum NodeIdOrSubgraph {
    Subgraph(StmtSubgraph),
    NodeId(NodeId),
}

impl Parse for NodeIdOrSubgraph {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(kw::subgraph) || input.peek(token::Brace) {
            return Ok(Self::Subgraph(input.parse()?));
        }
        Ok(Self::NodeId(input.parse()?))
    }
}

impl Printable for NodeIdOrSubgraph {
    fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result {
        match self {
            NodeIdOrSubgraph::Subgraph(it) => f.print(it),
            NodeIdOrSubgraph::NodeId(it) => f.print(it),
        }
    }
}

#[cfg(test)]
impl NodeIdOrSubgraph {
    fn ident(s: &str) -> Self {
        Self::NodeId(NodeId {
            id: ID::ident(s),
            port: None,
        })
    }
}

#[derive(ToTokens, Parse)]
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub enum EdgeOp {
    #[peek(pun::DirectedEdge, name = "->")]
    Directed(pun::DirectedEdge),
    #[peek(Token![-], name = "--")]
    Undirected(UndirectedEdge),
}

impl Printable for EdgeOp {
    fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result {
        match self {
            EdgeOp::Directed(it) => f.print(it),
            EdgeOp::Undirected(_) => f.print("--"),
        }
    }
}

#[derive(ToTokens, Parse)]
#[cfg_attr(test, derive(Debug, PartialEq, Eq, Default))]
pub struct UndirectedEdge(Token![-], Token![-]);

#[test]
#[should_panic = "expected `--`"]
fn custom_punct_for_directed_edge_does_not_work() {
    // cannot use this because the lexer will always give us Alone, Alone, which isn't parsed
    syn::custom_punctuation!(Demo, --);
    let _: Demo = syn::parse_quote!(--);
}

#[cfg(test)]
impl EdgeOp {
    /// ->
    fn directed() -> Self {
        Self::Directed(tok::directed_edge())
    }
    /// --
    fn undirected() -> Self {
        Self::Undirected(tok::undirected_edge())
    }
}

#[derive(ToTokens, Parse)]
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub struct StmtNode {
    pub node_id: NodeId,
    #[peek(token::Bracket)]
    pub attributes: Option<Attributes>,
}

impl Printable for StmtNode {
    fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result {
        let Self {
            node_id,
            attributes,
        } = self;
        f.print(node_id)?;
        f.print(attributes)
    }
}

#[test]
fn parse_stmt_node() {
    assert_eq!(
        StmtNode {
            node_id: NodeId {
                id: ID::ident("noddy"),
                port: None
            },
            attributes: None
        },
        syn::parse_quote!(noddy)
    );
    assert_eq!(
        StmtNode {
            node_id: NodeId {
                id: ID::ident("noddy"),
                port: None
            },
            attributes: Some(Attributes {
                lists: vec![AttrList {
                    bracket_token: tok::bracket(),
                    kvs: vec![]
                }]
            })
        },
        syn::parse_quote!(noddy[])
    );
    assert_eq!(
        StmtNode {
            node_id: NodeId {
                id: ID::ident("noddy"),
                port: None
            },
            attributes: Some(Attributes {
                lists: vec![AttrList {
                    bracket_token: tok::bracket(),
                    kvs: vec![AttrKV::ident_and_string("label", "make way for noddy")]
                }]
            })
        },
        syn::parse_quote!(noddy[label = "make way for noddy"])
    );
}

#[derive(ToTokens, Parse)]
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub struct NodeId {
    pub id: ID,
    #[peek(token::Colon)]
    pub port: Option<Port>,
}

impl Printable for NodeId {
    fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result {
        let Self { id, port } = self;
        f.print(id)?;
        f.print(port)
    }
}

#[test]
fn parse_node_id() {
    assert_eq!(
        NodeId {
            id: ID::ident("noddy"),
            port: None
        },
        syn::parse_quote!(noddy),
    );
    assert_eq!(
        NodeId {
            id: ID::ident("noddy"),
            port: Some(Port::ID {
                colon: tok::colon(),
                id: ID::lit_str("some port")
            })
        },
        syn::parse_quote!(noddy:"some port"),
    );
    assert_eq!(
        NodeId {
            id: ID::ident("noddy"),
            port: Some(Port::Compass {
                colon: tok::colon(),
                compass: CompassPoint::C(tok::c())
            })
        },
        syn::parse_quote!(noddy:c),
    );
}

#[derive(ToTokens)]
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub enum Port {
    ID {
        colon: Token![:],
        id: ID,
    },
    Compass {
        colon: Token![:],
        compass: CompassPoint,
    },
    IDAndCompass {
        colon1: Token![:],
        id: ID,
        colon2: Token![:],
        compass: CompassPoint,
    },
}

impl Parse for Port {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let colon = input.parse()?;
        if input.fork().parse::<CompassPoint>().is_ok() {
            return Ok(Self::Compass {
                colon,
                compass: input.parse()?,
            });
        }
        let id = input.parse()?;
        match input.peek(Token![:]) {
            false => Ok(Self::ID { colon, id }),
            true => Ok(Self::IDAndCompass {
                colon1: colon,
                id,
                colon2: input.parse()?,
                compass: input.parse()?,
            }),
        }
    }
}

impl Printable for Port {
    fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result {
        match self {
            Port::ID { colon, id } => {
                f.print(colon)?;
                f.print(id)
            }
            Port::Compass { colon, compass } => {
                f.print(colon)?;
                f.print(compass)
            }
            Port::IDAndCompass {
                colon1,
                id,
                colon2,
                compass,
            } => {
                f.print(colon1)?;
                f.print(id)?;
                f.print(colon2)?;
                f.print(compass)
            }
        }
    }
}

#[derive(Parse)]
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub struct StmtSubgraph {
    #[call(Self::parse_prelude)]
    pub prelude: Option<(kw::subgraph, Option<ID>)>,
    #[brace]
    pub brace_token: token::Brace,
    #[inside(brace_token)]
    pub statements: Statements,
}

impl StmtSubgraph {
    fn parse_prelude(input: ParseStream) -> syn::Result<Option<(kw::subgraph, Option<ID>)>> {
        if input.peek(token::Brace) {
            return Ok(None);
        }
        let subgraph = input.parse()?;
        if input.peek(token::Brace) {
            return Ok(Some((subgraph, None)));
        }
        Ok(Some((subgraph, Some(input.parse()?))))
    }
}

impl ToTokens for StmtSubgraph {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            prelude,
            brace_token,
            statements,
        } = self;
        if let Some((kw, id)) = prelude {
            kw.to_tokens(tokens);
            id.to_tokens(tokens)
        }
        brace_token.surround(tokens, |inner| statements.to_tokens(inner))
    }
}

impl Printable for StmtSubgraph {
    fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result {
        let Self {
            prelude,
            brace_token,
            statements,
        } = self;
        f.print(kw::subgraph::default())?; // always
        if let Some((_subgraph, Some(id))) = prelude {
            f.sep()?;
            f.print(id)?
        }
        f.sep()?;
        f.delimit(brace_token, |f| f.print(statements))
    }
}

enum_of_kws!(
    pub enum CompassPoint {
        #[name = "n"]
        N(kw::n),
        #[name = "ne"]
        NE(kw::ne),
        #[name = "e"]
        E(kw::e),
        #[name = "se"]
        SE(kw::se),
        #[name = "s"]
        S(kw::s),
        #[name = "sw"]
        SW(kw::sw),
        #[name = "w"]
        W(kw::w),
        #[name = "nw"]
        NW(kw::nw),
        #[name = "c"]
        C(kw::c),
    }
);

#[derive(ToTokens)]
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub enum ID {
    AnyIdent(syn::Ident),
    AnyLit(syn::Lit),
    Html(HtmlString),
    DotInt(DotInt),
}

impl Printable for ID {
    fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result {
        match self {
            ID::AnyIdent(it) => f.print(it),
            ID::AnyLit(it) => f.print(it),
            ID::Html(it) => f.print(it),
            ID::DotInt(it) => f.print(it),
        }
    }
}

impl Parse for ID {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(syn::Ident::peek_any) {
            return Ok(Self::AnyIdent(input.call(syn::Ident::parse_any)?));
        }
        if input.peek(syn::Lit) {
            return Ok(Self::AnyLit(input.parse()?));
        }
        if input.peek(Token![<]) {
            return Ok(Self::Html(input.parse()?));
        }
        if input.peek(Token![.]) {
            return Ok(Self::DotInt(input.parse()?));
        }
        Err(input.error("expected an identifier, literal or HTML string"))
    }
}

#[test]
fn parse_id() {
    assert_eq!(ID::lit_str("stringy"), syn::parse_quote!("stringy"));
    assert_eq!(ID::ident("identy"), syn::parse_quote!(identy));
}

#[cfg(test)]
impl ID {
    fn lit_str(s: &str) -> Self {
        Self::AnyLit(syn::Lit::Str(syn::LitStr::new(
            s,
            proc_macro2::Span::call_site(),
        )))
    }
    fn ident(s: &str) -> Self {
        Self::AnyIdent(syn::Ident::new(s, proc_macro2::Span::call_site()))
    }
}

#[derive(Parse, ToTokens)]
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub struct DotInt {
    pub dot: Token![.],
    pub int: syn::LitInt,
}

impl Printable for DotInt {
    fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result {
        let Self { dot: _, int } = self;
        f.write_fmt(format_args!(".{int}"))
    }
}

#[derive(ToTokens)]
#[cfg_attr(test, derive(Debug))]
pub struct HtmlString {
    pub lt: Token![<],
    pub stream: TokenStream,
}

impl Printable for HtmlString {
    fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result {
        let Self { lt: _, stream } = self;
        f.write_fmt(format_args!("< {stream}"))
    }
}

#[cfg(test)]
impl PartialEq for HtmlString {
    fn eq(&self, other: &Self) -> bool {
        self.lt == other.lt && self.stream.to_string() == other.stream.to_string()
    }
}

#[cfg(test)]
impl Eq for HtmlString {}

impl Parse for HtmlString {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        use proc_macro2::TokenTree::Punct;

        let lt = input.parse()?;
        let mut nesting = 1usize;
        input.step(|cursor| {
            let mut stream = TokenStream::new();
            let mut rest = *cursor;
            while let Some((tt, next)) = rest.token_tree() {
                match &tt {
                    Punct(p) if p.as_char() == '>' => nesting -= 1,
                    Punct(p) if p.as_char() == '<' => nesting += 1,
                    _ => {}
                };
                rest = next;
                stream.extend([tt]);
            }
            if nesting == 0 {
                return Ok((Self { lt, stream }, syn::buffer::Cursor::empty()));
            }
            Err(cursor.error("unmatched `<` in html string"))
        })
    }
}

#[test]
fn parse_html_string() {
    use quote::quote;
    assert_eq!(
        HtmlString {
            lt: tok::lt(),
            stream: quote!(hello>)
        },
        syn::parse_quote!(<hello>)
    );
    assert_eq!(
        HtmlString {
            lt: tok::lt(),
            stream: quote!(hello <div> I am in a div </div> >)
        },
        syn::parse_quote!(<hello <div> I am in a div </div> >)
    );
}

#[cfg(test)]
mod tok {
    use super::{kw, pun};
    use syn::token;

    crate::tok!(
        bracket -> token::Bracket,
        c -> kw::c,
        colon -> token::Colon,
        directed_edge -> pun::DirectedEdge,
        eq -> token::Eq,
        lt -> token::Lt,
        undirected_edge -> super::UndirectedEdge
    );
}
