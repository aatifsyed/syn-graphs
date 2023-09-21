pub mod dot;
pub mod mermaid;

macro_rules! enum_of_kws {
    (
        pub enum $this:ident {
            $(
                #[name = $lit:literal]
                $variant:ident($inner:ty)
            ),* $(,)?
        }
    ) => {
        #[derive(derive_syn_parse::Parse, derive_quote_to_tokens::ToTokens, derive_more::Display)]
        #[cfg_attr(test, derive(Debug, PartialEq, Eq))]
        pub enum $this {
            $(
                #[display(fmt = $lit)]
                #[peek($inner, name = $lit)] // stringify!($inner) doesn't work here
                $variant($inner),
            )*
        }

        impl crate::Printable for $this {
            fn print_to<T: std::fmt::Write>(&self, f: &mut indenter::CodeFormatter<'_, T>) -> std::fmt::Result {
                use std::fmt::Write as _;
                match self {
                    $(
                        $this::$variant(_) => f.write_str($lit),
                    )*
                }
            }
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
use indenter::CodeFormatter;
#[cfg(test)]
pub(crate) use tok;

use std::fmt::{self, Write as _};
use syn::token;

trait CodeFormatterExt<'a, W: fmt::Write + 'a>: fmt::Write + 'a {
    fn get_code_formatter(&mut self) -> &mut CodeFormatter<'a, W>;
    fn sep(&mut self) -> fmt::Result {
        self.write_char(' ')
    }
    fn newline(&mut self) -> fmt::Result {
        self.write_char('\n')
    }
    fn print(&mut self, it: impl Printable) -> fmt::Result {
        it.print_to(self.get_code_formatter())
    }
    fn delimit<D: TreeDelimiter>(
        &mut self,
        _: &D, // use it
        inside: impl FnOnce(&mut CodeFormatter<'_, W>) -> fmt::Result,
    ) -> fmt::Result {
        self.write_char(D::OPEN)?;
        self.get_code_formatter().indent(1);
        inside(self.get_code_formatter())?;
        self.get_code_formatter().dedent(1);
        self.write_char(D::CLOSE)
    }
}
impl<'a, W: fmt::Write> CodeFormatterExt<'a, W> for CodeFormatter<'a, W> {
    fn get_code_formatter(&mut self) -> &mut CodeFormatter<'a, W> {
        self
    }
}

trait Printable {
    fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result;
}

impl Printable for syn::Lit {
    fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result {
        let token = match self {
            syn::Lit::Str(it) => it.token(),
            syn::Lit::ByteStr(it) => it.token(),
            syn::Lit::Byte(it) => it.token(),
            syn::Lit::Char(it) => it.token(),
            syn::Lit::Int(it) => it.token(),
            syn::Lit::Float(it) => it.token(),
            syn::Lit::Verbatim(it) => it.clone(),
            syn::Lit::Bool(it) => return f.write_fmt(format_args!("{}", it.value)),
            _ => return Err(fmt::Error),
        };
        f.write_fmt(format_args!("{}", token))
    }
}

impl<T> Printable for Option<T>
where
    T: Printable,
{
    fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result {
        match self {
            Some(it) => it.print_to(f),
            None => Ok(()),
        }
    }
}

impl<T: Printable> Printable for &T {
    fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result {
        (*self).print_to(f)
    }
}

impl Printable for &str {
    fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result {
        f.write_str(self)
    }
}

macro_rules! impl_print {
    ($($ty:ty = $lit:literal),* $(,)?) => {
        $(
            impl Printable for $ty {
                fn print_to<T: fmt::Write>(&self, f: &mut CodeFormatter<'_, T>) -> fmt::Result {
                    f.write_str($lit)
                }
            }
        )*
    };
}
impl_print!(
    token::Comma = ",",
    token::Colon = ":",
    token::Eq = "=",
    token::Semi = ";"
);

impl Printable for syn::Ident {
    fn print_to<W: fmt::Write>(&self, f: &mut CodeFormatter<'_, W>) -> fmt::Result {
        f.write_fmt(format_args!("{}", self))
    }
}

trait TreeDelimiter {
    const OPEN: char;
    const CLOSE: char;
}

macro_rules! tree_delimiter {
    ($($ty:ty { $open:literal $close:literal }),* $(,)?) => {
        $(
            impl TreeDelimiter for $ty { const OPEN: char = $open; const CLOSE: char = $close; }
        )*
    };
}

tree_delimiter!(token::Brace { '{' '}' }, token::Paren { '(' ')' }, token::Bracket { '[' ']' });
