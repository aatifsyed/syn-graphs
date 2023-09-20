use derive_syn_parse::Parse;
#[cfg(test)]
use pretty_assertions::assert_eq;
use syn::{
    ext::IdentExt as _,
    parse::{Parse, ParseStream},
    token, Token,
};

pub mod kw {
    syn::custom_keyword!(strict);
    syn::custom_keyword!(graph);
    syn::custom_keyword!(digraph);
    syn::custom_keyword!(node);
    syn::custom_keyword!(edge);
    syn::custom_keyword!(subgraph);
    syn::custom_keyword!(n);
    syn::custom_keyword!(ne);
    syn::custom_keyword!(e);
    syn::custom_keyword!(se);
    syn::custom_keyword!(s);
    syn::custom_keyword!(sw);
    syn::custom_keyword!(w);
    syn::custom_keyword!(nw);
    syn::custom_keyword!(c);
}

macro_rules! enum_of_kws {
    (
        pub enum $this:ident {
            $(
                #[name = $lit:literal]
                $variant:ident($inner:ty)
            ),* $(,)?
        }
    ) => {
        #[derive(Parse)]
        #[cfg_attr(test, derive(Debug, PartialEq, Eq))]
        pub enum $this {
            $(
                // stringify!($inner) doesn't work here
                #[peek($inner, name = $lit)]
                $variant($inner),
            )*
        }
    };
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

#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
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
        if input.peek2(Token![-]) {
            // must be an edgeop
            return Ok(Self::Edge(input.parse()?));
        }
        if input.peek(kw::subgraph) || input.peek(token::Brace) {
            return Ok(Self::Subgraph(input.parse()?));
        }
        Ok(Self::Node(input.parse()?))
    }
}

#[derive(Parse)]
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub struct StmtAssign {
    pub left: ID,
    pub eq_token: Token![=],
    pub right: ID,
}

#[derive(Parse)]
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub struct StmtAttr {
    pub on: StmtAttrOn,
    pub attributes: Attributes,
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

#[derive(Parse)]
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub struct AttrKV {
    pub left: ID,
    pub eq_token: Token![=],
    pub right: ID,
    #[call(Self::parse_sep)]
    pub trailing: Option<AttrSep>,
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
        #[name = "comma"]
        Comma(token::Comma),
        #[name = "semi"]
        Semi(token::Semi),
    }
);

#[derive(Parse)]
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub struct StmtEdge {
    pub id: ID,
    /// Non-empty
    #[call(Self::parse_ops)]
    pub ops: Vec<(EdgeOp, ID)>,
    #[peek(token::Bracket)]
    pub attrs: Option<Attributes>,
}

impl StmtEdge {
    fn parse_ops(input: ParseStream) -> syn::Result<Vec<(EdgeOp, ID)>> {
        let mut ops = vec![(input.parse()?, input.parse()?)];
        while input.peek(Token![-]) {
            ops.push((input.parse()?, input.parse()?))
        }
        Ok(ops)
    }
}

#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub enum EdgeOp {
    Directed { dash: Token![-], gt: Token![>] },
    Undirected { dash1: Token![-], dash2: Token![-] },
}

impl Parse for EdgeOp {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let dash = input.parse()?;
        match input.peek(Token![-]) {
            true => Ok(Self::Undirected {
                dash1: dash,
                dash2: input.parse()?,
            }),
            false => Ok(Self::Directed {
                dash,
                gt: input.parse()?,
            }),
        }
    }
}

#[derive(Parse)]
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub struct StmtNode {
    pub node_id: NodeId,
    #[peek(token::Bracket)]
    pub attributes: Option<Attributes>,
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

#[derive(Parse)]
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
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

#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub enum ID {
    AnyIdent(syn::Ident),
    AnyLit(syn::Lit),
}

impl Parse for ID {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(syn::Ident::peek_any) {
            return Ok(Self::AnyIdent(input.call(syn::Ident::parse_any)?));
        }
        if input.peek(syn::Lit) {
            return Ok(Self::AnyLit(input.parse()?));
        }
        Err(input.error("expected an identifier or literal"))
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

#[cfg(test)]
mod tok {
    use super::kw;
    use syn::token;

    macro_rules! tok {
        ($($fn_name:ident -> $ty_name:ty),* $(,)?) => {
            $(
                pub fn $fn_name() -> $ty_name { Default::default() }
            )*
        };
    }
    tok!(colon -> token::Colon, bracket -> token::Bracket, c -> kw::c, eq -> token::Eq);
}
