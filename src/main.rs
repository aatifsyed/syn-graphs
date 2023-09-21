use std::{error::Error, fs::File, io::Read, str::FromStr};

use proc_macro2::TokenStream;
use syn::parse::{Parse as _, Parser as _};

fn main() -> Result<(), Box<dyn Error>> {
    let mut s = String::new();
    File::open("/dev/stdin")?.read_to_string(&mut s)?;
    let ts = TokenStream::from_str(&s)?;
    let graph = syn_graphs::dot::Graph::parse.parse2(ts)?;
    println!("{}", graph);
    Ok(())
}
