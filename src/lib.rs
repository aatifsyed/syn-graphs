pub mod dot;
pub mod pet;
pub mod unparse;

// transplant the unparsing logic from prettyplease

use prettyplease::{algorithm, ring};

const MARGIN: isize = 89;
const INDENT: isize = 4;
const MIN_SPACE: isize = 60;

#[allow(unused)]
mod prettyplease {
    pub(crate) mod algorithm;
    pub(crate) mod convenience;
    pub(crate) mod iter;
    pub(crate) mod lit;
    pub(crate) mod ring;
    pub(crate) mod token;
}

macro_rules! enum_of_kws {
    (
        $(#[$meta:meta])*
        pub enum $this:ident {
            $(
                #[name = $lit:literal]
                $variant:ident($inner:ty)
            ),* $(,)?
        }
    ) => {
        $(#[$meta])*
        #[derive(
            derive_syn_parse::Parse,
            derive_quote_to_tokens::ToTokens,
            derive_more::Display,
            Debug,
            PartialEq,
            Eq,
            Clone,
            Hash,
        )]
        pub enum $this {
            $(
                // stringify!($inner) doesn't work here
                #[peek($inner, name = $lit)]
                #[display(fmt = $lit)]
                $variant($inner),
            )*
        }
    };
}
pub(crate) use enum_of_kws;

#[cfg(test)]
macro_rules! tok {
    ($($fn_name:ident -> $ty_name:ty),* $(,)?) => {
        $(
            pub fn $fn_name() -> $ty_name { Default::default() }
        )*
    };
}
#[cfg(test)]
pub(crate) use tok;
