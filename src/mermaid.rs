use derive_syn_parse::Parse;
use proc_macro2::{Ident, TokenStream};
use syn::{
    ext::IdentExt as _,
    parse::{Parse, ParseStream},
    token, LitStr, Token,
};

use super::enum_of_kws;

pub mod kw {
    syn::custom_keyword!(flowchart);
    syn::custom_keyword!(graph);
    syn::custom_keyword!(direction);
    syn::custom_keyword!(TD);
    syn::custom_keyword!(TB);
    syn::custom_keyword!(BT);
    syn::custom_keyword!(RL);
    syn::custom_keyword!(LR);
    syn::custom_keyword!(x);
    syn::custom_keyword!(o);
}

enum_of_kws!(
    pub enum FlowchartOrGraph {
        #[name = "flowchart"]
        Flowchart(kw::flowchart),
        #[name = "graph"]
        Graph(kw::graph),
    }
);

enum_of_kws!(
    pub enum Direction {
        #[name = "TD"]
        TD(kw::TD),
        #[name = "TB"]
        TB(kw::TB),
        #[name = "BT"]
        BT(kw::BT),
        #[name = "RL"]
        RL(kw::RL),
        #[name = "LR"]
        LR(kw::LR),
    }
);

pub struct Node {
    pub ident: syn::Ident,
    pub text: Option<(TextContainerStyle, TokenStream)>,
}

impl Parse for Node {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.call(Ident::parse_any)?;

        if input.peek(token::Bracket) {
            let mut content;
            let bracket = syn::bracketed!(content in input);

            if content.peek(token::Bracket) {
                let bracket2 = syn::bracketed!(content in content);
                return Ok(Node {
                    ident,
                    text: Some((
                        TextContainerStyle::Subroutine(bracket, bracket2),
                        content.parse()?,
                    )),
                });
            }
            if content.peek(token::Paren) {
                let paren = syn::parenthesized!(content in content);
                return Ok(Node {
                    ident,
                    text: Some((
                        TextContainerStyle::Cylinder(bracket, paren),
                        content.parse()?,
                    )),
                });
            }
            return Ok(Node {
                ident,
                text: Some((TextContainerStyle::Box(bracket), content.parse()?)),
            });
        }

        if input.peek(token::Paren) {
            let mut content;
            let paren = syn::parenthesized!(content in input);

            if content.peek(token::Paren) {
                let paren2 = syn::parenthesized!(content in content);

                if content.peek(token::Paren) {
                    let paren3 = syn::parenthesized!(content in content);

                    return Ok(Node {
                        ident,
                        text: Some((
                            TextContainerStyle::DoubleCircle(paren, paren2, paren3),
                            content.parse()?,
                        )),
                    });
                }

                return Ok(Node {
                    ident,
                    text: Some((TextContainerStyle::Circle(paren, paren2), content.parse()?)),
                });
            }

            if content.peek(token::Bracket) {
                let bracket = syn::bracketed!(content in content);

                return Ok(Node {
                    ident,
                    text: Some((TextContainerStyle::Stadium(paren, bracket), input.parse()?)),
                });
            }

            return Ok(Node {
                ident,
                text: Some((TextContainerStyle::Round(paren), input.parse()?)),
            });
        }

        if input.peek(token::Brace) {
            let mut content;
            let brace = syn::braced!(content in input);

            if input.peek(token::Brace) {
                let brace2 = syn::braced!(content in content);
                return Ok(Node {
                    ident,
                    text: Some((TextContainerStyle::Hex(brace, brace2), input.parse()?)),
                });
            }

            return Ok(Node {
                ident,
                text: Some((TextContainerStyle::Rhombus(brace), input.parse()?)),
            });
        }

        Ok(Node { ident, text: None })
    }
}

pub enum TextContainerStyle {
    Box(token::Bracket),
    Subroutine(token::Bracket, token::Bracket),
    Cylinder(token::Bracket, token::Paren),
    // /// Punct is a forward slash
    // Parallelogram(token::Bracket, Punct),
    Round(token::Paren),
    Circle(token::Paren, token::Paren),
    DoubleCircle(token::Paren, token::Paren, token::Paren),
    Stadium(token::Paren, token::Bracket),

    Rhombus(token::Brace),
    Hex(token::Brace, token::Brace),
}

pub enum Link {
    Plain(LinkPlain),
    Labelled(LinkLabelled),
}

impl Parse for Link {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        match input.peek3(LitStr) {
            true => Ok(Self::Labelled(input.parse()?)),
            false => Ok(Self::Plain(input.parse()?)),
        }
    }
}

#[derive(Parse)]
pub struct LinkPlain {
    pub first: Token![-],
    pub second: Token![-],
    pub head: Token![>],
}

#[derive(Parse)]
pub struct LinkLabelled {
    pub first: Token![-],
    pub second: Token![-],
    pub label: LitStr,
    pub third: Token![-],
    pub fourth: Token![-],
    pub head: Token![>],
}
