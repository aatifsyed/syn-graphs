pub mod dot;

macro_rules! enum_of_kws {
    (
        pub enum $this:ident {
            $(
                #[name = $lit:literal]
                $variant:ident($inner:ty)
            ),* $(,)?
        }
    ) => {
        #[derive(derive_syn_parse::Parse, derive_quote_to_tokens::ToTokens)]
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
