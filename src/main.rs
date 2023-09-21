use std::{error::Error, str::FromStr};

use proc_macro2::TokenStream;
use syn::parse::{Parse as _, Parser as _};

fn main() -> Result<(), Box<dyn Error>> {
    let source = std::fs::read_to_string("/dev/stdin")?;
    let tokens = TokenStream::from_str(&source)?;
    let ast = syn_graphs::dot::Graph::parse.parse2(tokens)?;
    // println!("{}", syn_graphs::unparse::dot(&ast));
    Ok(())
}
