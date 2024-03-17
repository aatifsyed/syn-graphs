use crate::enum_of_kws;
use derive_quote_to_tokens::ToTokens;
use derive_syn_parse::Parse;
#[cfg(test)]
use pretty_assertions::assert_eq;
use proc_macro2::TokenStream;
use quote::ToTokens;
use std::{
    cmp::{self, Ord, Ordering, PartialOrd},
    hash::{Hash, Hasher},
};
use syn::{
    ext::IdentExt as _,
    parse::{Parse, ParseStream},
    spanned::Spanned as _,
    token, Token,
};

pub mod kw {
    macro_rules! custom_keywords {
        ($($ident:ident),* $(,)?) => {
            $(
                ::syn::custom_keyword!($ident);
                impl ::std::cmp::PartialOrd for $ident {
                    fn partial_cmp(&self, other: &Self) -> Option<::std::cmp::Ordering> {
                        Some(self.cmp(other))
                    }
                }
                impl ::std::cmp::Ord for $ident {
                    fn cmp(&self, _: &Self) -> ::std::cmp::Ordering {
                        ::std::cmp::Ordering::Equal
                    }
                }
            )*
        };
    }

    custom_keywords! {
        strict, graph, digraph, node, edge, subgraph, n, ne, e, se, s, sw, w, nw, c
    }
}

pub mod pun {
    use std::cmp::{Ord, Ordering, PartialOrd};

    syn::custom_punctuation!(DirectedEdge, ->);

    impl Ord for DirectedEdge {
        fn cmp(&self, _: &Self) -> Ordering {
            Ordering::Equal
        }
    }
    impl PartialOrd for DirectedEdge {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }
}

#[derive(Parse, Debug, PartialEq, Eq, Clone, Hash)]
pub struct Graph {
    pub strict: Option<kw::strict>,
    pub directedness: GraphDirectedness,
    #[call(Self::maybe_id)]
    pub id: Option<ID>,
    #[brace]
    pub brace_token: token::Brace,
    #[inside(brace_token)]
    pub stmt_list: StmtList,
}

impl Ord for Graph {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let Self {
            strict,
            directedness,
            id,
            brace_token: _,
            stmt_list,
        } = self;
        Ordering::Equal
            .then(strict.cmp(&other.strict))
            .then(directedness.cmp(&other.directedness))
            .then(id.cmp(&other.id))
            .then(Ordering::Equal /* brace_token */)
            .then(stmt_list.cmp(&other.stmt_list))
    }
}

impl PartialOrd for Graph {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
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
            directedness,
            id,
            brace_token,
            stmt_list,
        } = self;
        strict.to_tokens(tokens);
        directedness.to_tokens(tokens);
        id.to_tokens(tokens);
        brace_token.surround(tokens, |inner| stmt_list.to_tokens(inner))
    }
}

enum_of_kws!(
    #[derive(PartialOrd, Ord)]
    pub enum GraphDirectedness {
        #[name = "graph"]
        Graph(kw::graph),
        #[name = "digraph"]
        Digraph(kw::digraph),
    }
);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct StmtList {
    pub stmts: Vec<(Stmt, Option<Token![;]>)>,
}

impl Ord for StmtList {
    fn cmp(&self, other: &Self) -> Ordering {
        let Self { stmts } = self;

        // copied from stdlib
        let l = cmp::min(stmts.len(), other.stmts.len());

        // Slice to the loop iteration range to enable bound check
        // elimination in the compiler
        let lhs = &stmts[..l];
        let rhs = &other.stmts[..l];

        for i in 0..l {
            let (left_stmt, left_semi) = &lhs[i];
            let (right_stmt, right_semi) = &rhs[i];
            match (left_stmt, left_semi.map(|_| ())).cmp(&(right_stmt, right_semi.map(|_| ()))) {
                Ordering::Equal => (),
                non_eq => return non_eq,
            }
        }

        stmts.len().cmp(&other.stmts.len())
    }
}

impl PartialOrd for StmtList {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Parse for StmtList {
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
        Ok(Self { stmts: list })
    }
}

impl ToTokens for StmtList {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { stmts } = self;
        for (stmt, semi) in stmts {
            stmt.to_tokens(tokens);
            semi.to_tokens(tokens)
        }
    }
}
#[derive(Clone, Debug, PartialEq, Eq, ToTokens, Hash, PartialOrd, Ord)]
pub enum Stmt {
    Attr(StmtAttr),
    Assign(StmtAssign),
    Node(StmtNode),
    Edge(StmtEdge),
    Subgraph(StmtSubgraph),
}

impl Parse for Stmt {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.fork().parse::<AttrTarget>().is_ok() {
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

#[test]
fn parse_stmt() {
    assert_eq!(
        Stmt::Edge(StmtEdge {
            from: EdgeTarget::NodeId(NodeId {
                id: ID::lit_str("node0"),
                port: Some(Port::ID {
                    colon: tok::colon(),
                    id: ID::ident("f0")
                })
            }),
            edges: vec![(
                EdgeDirectedness::directed(),
                EdgeTarget::NodeId(NodeId {
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
#[derive(Clone, ToTokens, Parse, Debug, PartialEq, Eq, Hash)]
pub struct StmtAssign {
    pub left: ID,
    pub eq_token: Token![=],
    pub right: ID,
}

impl Ord for StmtAssign {
    fn cmp(&self, other: &Self) -> Ordering {
        let Self {
            left,
            eq_token: _,
            right,
        } = self;
        Ordering::Equal
            .then(left.cmp(&other.left))
            .then(Ordering::Equal /* eq_token */)
            .then(right.cmp(&other.right))
    }
}

impl PartialOrd for StmtAssign {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Parse, ToTokens, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct StmtAttr {
    pub target: AttrTarget,
    pub attrs: Attrs,
}

enum_of_kws!(
    #[derive(PartialOrd, Ord)]
    pub enum AttrTarget {
        #[name = "graph"]
        Graph(kw::graph),
        #[name = "node"]
        Node(kw::node),
        #[name = "edge"]
        Edge(kw::edge),
    }
);

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Attrs {
    /// Non-empty
    pub lists: Vec<AttrList>,
}

impl Parse for Attrs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut lists = vec![input.parse()?];
        while input.peek(token::Bracket) {
            lists.push(input.parse()?)
        }
        Ok(Self { lists })
    }
}

impl ToTokens for Attrs {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { lists } = self;
        for list in lists {
            list.to_tokens(tokens)
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct AttrList {
    pub bracket_token: token::Bracket,
    pub assigns: Vec<AttrAssign>,
}

impl Ord for AttrList {
    fn cmp(&self, other: &Self) -> Ordering {
        let Self {
            bracket_token: _,
            assigns,
        } = self;
        assigns.cmp(&other.assigns)
    }
}

impl PartialOrd for AttrList {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Parse for AttrList {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut assigns = vec![];
        let content;
        let bracket_token = syn::bracketed!(content in input);
        while !content.is_empty() {
            assigns.push(content.parse()?)
        }
        Ok(Self {
            bracket_token,
            assigns,
        })
    }
}

impl ToTokens for AttrList {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            bracket_token,
            assigns,
        } = self;
        bracket_token.surround(tokens, |inner| {
            for assign in assigns {
                assign.to_tokens(inner)
            }
        })
    }
}

#[test]
fn parse_attr_list_penultimate_html() {
    assert_eq!(
        AttrList {
            bracket_token: tok::bracket(),
            assigns: vec![
                AttrAssign::standalone(ID::ident("color"), ID::lit_str("#88000022")),
                AttrAssign::standalone(
                    ID::ident("label"),
                    ID::html(quote::quote!(<em>hello!</em>))
                ),
                AttrAssign::standalone(ID::ident("shape"), ID::ident("plain")),
            ],
        },
        syn::parse_quote!(
            [
                color="#88000022"
                label=<<em>hello!</em>>
                shape=plain
            ]
        )
    );
}

#[derive(Clone, ToTokens, Parse, Debug, PartialEq, Eq, Hash)]
pub struct AttrAssign {
    pub left: ID,
    pub eq_token: Token![=],
    pub right: ID,
    #[call(Self::parse_sep)]
    pub trailing: Option<AttrSep>,
}

impl Ord for AttrAssign {
    fn cmp(&self, other: &Self) -> Ordering {
        let Self {
            left,
            eq_token: _,
            right,
            trailing,
        } = self;
        Ordering::Equal
            .then(left.cmp(&other.left))
            .then(right.cmp(&other.right))
            .then(trailing.cmp(&other.trailing))
    }
}

impl PartialOrd for AttrAssign {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[cfg(test)]
impl AttrAssign {
    fn standalone(left: ID, right: ID) -> Self {
        Self {
            left,
            eq_token: tok::eq(),
            right,
            trailing: None,
        }
    }
}

impl AttrAssign {
    fn parse_sep(input: ParseStream) -> syn::Result<Option<AttrSep>> {
        if input.peek(Token![,]) || input.peek(Token![;]) {
            return Ok(Some(input.parse()?));
        }
        Ok(None)
    }
}

enum_of_kws!(
    pub enum AttrSep {
        #[name = "comma"]
        Comma(token::Comma),
        #[name = "semi"]
        Semi(token::Semi),
    }
);

impl Ord for AttrSep {
    fn cmp(&self, other: &Self) -> Ordering {
        fn discriminant(attr_sep: &AttrSep) -> u8 {
            match attr_sep {
                AttrSep::Comma(token::Comma { .. }) => 0,
                AttrSep::Semi(token::Semi { .. }) => 1,
            }
        }
        discriminant(self).cmp(&discriminant(other))
    }
}

impl PartialOrd for AttrSep {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Parse, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct StmtEdge {
    pub from: EdgeTarget,
    /// Non-empty
    #[call(Self::parse_edges)]
    pub edges: Vec<(EdgeDirectedness, EdgeTarget)>,
    #[peek(token::Bracket)]
    pub attrs: Option<Attrs>,
}

impl StmtEdge {
    fn parse_edges(input: ParseStream) -> syn::Result<Vec<(EdgeDirectedness, EdgeTarget)>> {
        let mut edges = vec![(input.parse()?, input.parse()?)];
        while input.peek(pun::DirectedEdge) || input.peek(Token![-]) {
            edges.push((input.parse()?, input.parse()?))
        }
        Ok(edges)
    }
}

#[test]
fn parse_stmt_edge() {
    assert_eq!(
        StmtEdge {
            from: EdgeTarget::ident("alice"),
            edges: vec![(EdgeDirectedness::undirected(), EdgeTarget::ident("bob"))],
            attrs: None
        },
        syn::parse_quote! {
            alice -- bob
        }
    )
}

impl ToTokens for StmtEdge {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { from, edges, attrs } = self;
        from.to_tokens(tokens);
        for (dir, to) in edges {
            dir.to_tokens(tokens);
            to.to_tokens(tokens)
        }
        attrs.to_tokens(tokens)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, ToTokens, Hash, PartialOrd, Ord)]
pub enum EdgeTarget {
    Subgraph(StmtSubgraph),
    NodeId(NodeId),
}

impl Parse for EdgeTarget {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(kw::subgraph) || input.peek(token::Brace) {
            return Ok(Self::Subgraph(input.parse()?));
        }
        Ok(Self::NodeId(input.parse()?))
    }
}

#[cfg(test)]
impl EdgeTarget {
    fn ident(s: &str) -> Self {
        Self::NodeId(NodeId {
            id: ID::ident(s),
            port: None,
        })
    }
}

#[derive(Clone, ToTokens, Parse, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum EdgeDirectedness {
    #[peek(pun::DirectedEdge, name = "->")]
    Directed(pun::DirectedEdge),
    #[peek(syn::token::Minus, name = "--")]
    Undirected(UndirectedEdge),
}

#[derive(Clone, ToTokens, Parse, Debug, PartialEq, Eq, Default, Hash)]
pub struct UndirectedEdge(Token![-], Token![-]);

impl Ord for UndirectedEdge {
    fn cmp(&self, _: &Self) -> Ordering {
        Ordering::Equal
    }
}

impl PartialOrd for UndirectedEdge {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[test]
#[should_panic = "expected `--`"]
fn custom_punct_for_directed_edge_does_not_work() {
    // cannot use this because the lexer will always give us Alone, Alone, which isn't parsed
    syn::custom_punctuation!(Demo, --);
    let _: Demo = syn::parse_quote!(--);
}

#[cfg(test)]
impl EdgeDirectedness {
    /// ->
    fn directed() -> Self {
        Self::Directed(tok::directed_edge())
    }
    /// --
    fn undirected() -> Self {
        Self::Undirected(tok::undirected_edge())
    }
}

#[derive(Clone, ToTokens, Parse, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct StmtNode {
    pub node_id: NodeId,
    #[peek(token::Bracket)]
    pub attrs: Option<Attrs>,
}

#[test]
fn parse_stmt_node() {
    assert_eq!(
        StmtNode {
            node_id: NodeId {
                id: ID::ident("noddy"),
                port: None
            },
            attrs: None
        },
        syn::parse_quote!(noddy)
    );
    assert_eq!(
        StmtNode {
            node_id: NodeId {
                id: ID::ident("noddy"),
                port: None
            },
            attrs: Some(Attrs {
                lists: vec![AttrList {
                    bracket_token: tok::bracket(),
                    assigns: vec![]
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
            attrs: Some(Attrs {
                lists: vec![AttrList {
                    bracket_token: tok::bracket(),
                    assigns: vec![AttrAssign::standalone(
                        ID::ident("label"),
                        ID::lit_str("make way for noddy")
                    )]
                }]
            })
        },
        syn::parse_quote!(noddy[label = "make way for noddy"])
    );
}

#[derive(Clone, ToTokens, Parse, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NodeId {
    pub id: ID,
    #[peek(token::Colon)]
    pub port: Option<Port>,
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

#[derive(Clone, ToTokens, Debug, PartialEq, Eq, Hash)]
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

impl Ord for Port {
    fn cmp(&self, other: &Self) -> Ordering {
        fn port2options(port: &Port) -> (Option<&ID>, Option<&CompassPoint>) {
            match port {
                Port::ID { colon: _, id } => (Some(id), None),
                Port::Compass { colon: _, compass } => (None, Some(compass)),
                Port::IDAndCompass {
                    colon1: _,
                    id,
                    colon2: _,
                    compass,
                } => (Some(id), Some(compass)),
            }
        }
        port2options(self).cmp(&port2options(other))
    }
}

impl PartialOrd for Port {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
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

#[derive(Clone, Parse, Debug, PartialEq, Eq, Hash)]
pub struct StmtSubgraph {
    #[call(Self::parse_prelude)]
    pub prelude: Option<(kw::subgraph, Option<ID>)>,
    #[brace]
    pub brace_token: token::Brace,
    #[inside(brace_token)]
    pub statements: StmtList,
}

impl Ord for StmtSubgraph {
    fn cmp(&self, other: &Self) -> Ordering {
        let Self {
            prelude,
            brace_token: _,
            statements,
        } = self;
        Ordering::Equal
            .then(prelude.cmp(&other.prelude))
            .then(Ordering::Equal /* brace_token */)
            .then(statements.cmp(&other.statements))
    }
}

impl PartialOrd for StmtSubgraph {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
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

enum_of_kws!(
    #[derive(PartialOrd, Ord)]
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

#[derive(Clone, ToTokens, Debug, PartialEq, Eq, Hash)]
pub enum ID {
    AnyIdent(syn::Ident),
    AnyLit(syn::Lit),
    Html(HtmlString),
    DotInt(DotInt),
}

impl Ord for ID {
    fn cmp(&self, other: &Self) -> Ordering {
        fn id2options(
            id: &ID,
        ) -> (
            Option<&syn::Ident>,
            Option<String>,
            Option<&HtmlString>,
            Option<&DotInt>,
        ) {
            match id {
                ID::AnyIdent(it) => (Some(it), None, None, None),
                ID::AnyLit(it) => (None, Some(it.to_token_stream().to_string()), None, None),
                ID::Html(it) => (None, None, Some(it), None),
                ID::DotInt(it) => (None, None, None, Some(it)),
            }
        }
        id2options(self).cmp(&id2options(other))
    }
}

impl PartialOrd for ID {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
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
    fn html(stream: TokenStream) -> Self {
        Self::Html(HtmlString {
            lt: tok::lt(),
            stream,
            gt: tok::gt(),
        })
    }
}

#[derive(Clone, Parse, ToTokens, Debug, PartialEq, Eq, Hash)]
pub struct DotInt {
    pub dot: Token![.],
    pub int: syn::LitInt,
}

impl Ord for DotInt {
    fn cmp(&self, other: &Self) -> Ordering {
        let Self { dot: _, int } = self;
        int.to_token_stream()
            .to_string()
            .cmp(&other.int.to_token_stream().to_string())
    }
}

impl PartialOrd for DotInt {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, ToTokens, Debug)]
pub struct HtmlString {
    pub lt: Token![<],
    pub stream: TokenStream,
    pub gt: Token![>],
}

impl Ord for HtmlString {
    fn cmp(&self, other: &Self) -> Ordering {
        let Self {
            lt: _,
            stream,
            gt: _,
        } = self;
        stream.to_string().cmp(&other.stream.to_string())
    }
}

impl PartialOrd for HtmlString {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Hash for HtmlString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let Self { lt, stream, gt } = self;
        lt.hash(state);
        stream.to_string().hash(state);
        gt.hash(state);
    }
}

impl HtmlString {
    pub fn source(&self) -> Option<String> {
        self.stream.span().source_text()
    }
}

impl PartialEq for HtmlString {
    fn eq(&self, other: &Self) -> bool {
        self.lt == other.lt && self.stream.to_string() == other.stream.to_string()
    }
}

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
                    Punct(p) if p.as_char() == '>' => {
                        nesting -= 1;
                        if nesting == 0 {
                            return Ok((
                                Self {
                                    lt,
                                    stream,
                                    gt: syn::parse2(TokenStream::from(tt))
                                        .expect("just saw that this was a `>`"),
                                },
                                next,
                            ));
                        }
                    }
                    Punct(p) if p.as_char() == '<' => nesting += 1,
                    _ => {}
                };
                rest = next;
                stream.extend([tt]);
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
            stream: quote!(hello),
            gt: tok::gt(),
        },
        syn::parse_quote!(<hello>)
    );
    assert_eq!(
        HtmlString {
            lt: tok::lt(),
            stream: quote!(hello <div> I am in a div </div>),
            gt: tok::gt(),
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
        gt -> token::Gt,
        lt -> token::Lt,
        undirected_edge -> super::UndirectedEdge
    );
}
