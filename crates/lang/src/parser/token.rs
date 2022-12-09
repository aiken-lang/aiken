use std::fmt;

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum Token {
    Error(char),
    Name { name: String },
    UpName { name: String },
    DiscardName { name: String },
    Int { value: String },
    String { value: String },
    // Groupings
    LeftParen,   // (
    RightParen,  // )
    LeftSquare,  // [
    RightSquare, // }
    LeftBrace,   // {
    RightBrace,  // }
    // Int Operators
    Plus,
    Minus,
    Star,
    Slash,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Percent,
    // ByteString Operators
    PlusDot,         // '+.'
    MinusDot,        // '-.'
    StarDot,         // '*.'
    SlashDot,        // '/.'
    LessDot,         // '<.'
    GreaterDot,      // '>.'
    LessEqualDot,    // '<=.'
    GreaterEqualDot, // '>=.'
    // Other Punctuation
    Colon,
    Comma,
    Hash, // '#'
    Bang, // '!'
    Equal,
    EqualEqual, // '=='
    NotEqual,   // '!='
    Vbar,       // '|'
    VbarVbar,   // '||'
    AmperAmper, // '&&'
    Pipe,       // '|>'
    Dot,        // '.'
    RArrow,     // '->'
    DotDot,     // '..'
    EndOfFile,
    // Docs/Extra
    Comment,
    DocComment,
    ModuleComment,
    EmptyLine,
    // Keywords (alphabetically):
    As,
    Assert,
    Check,
    Const,
    Fn,
    If,
    Else,
    Is,
    Let,
    Opaque,
    Pub,
    Use,
    Test,
    Todo,
    Trace,
    Type,
    When,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Token::Error(c) => {
                write!(f, "\"{}\"", c)?;

                return Ok(());
            }
            Token::Name { name } => name,
            Token::UpName { name } => name,
            Token::DiscardName { name } => name,
            Token::Int { value } => value,
            Token::String { value } => value,
            Token::LeftParen => "(",
            Token::RightParen => ")",
            Token::LeftSquare => "[",
            Token::RightSquare => "]",
            Token::LeftBrace => "{",
            Token::RightBrace => "}",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Star => "*",
            Token::Slash => "/",
            Token::Less => "<",
            Token::Greater => ">",
            Token::LessEqual => "<=",
            Token::GreaterEqual => ">=",
            Token::Percent => "%",
            Token::PlusDot => "+.",
            Token::MinusDot => "-.",
            Token::StarDot => "*.",
            Token::SlashDot => "/.",
            Token::LessDot => "<.",
            Token::GreaterDot => ">.",
            Token::LessEqualDot => "<=.",
            Token::GreaterEqualDot => ">=.",
            Token::Colon => ":",
            Token::Comma => ",",
            Token::Hash => "#",
            Token::Bang => "!",
            Token::Equal => "=",
            Token::EqualEqual => "==",
            Token::NotEqual => "!=",
            Token::Vbar => "|",
            Token::VbarVbar => "||",
            Token::AmperAmper => "&&",
            Token::Pipe => "|>",
            Token::Dot => ".",
            Token::RArrow => "->",
            Token::DotDot => "..",
            Token::EndOfFile => "EOF",
            Token::Comment => "//",
            Token::DocComment => "///",
            Token::ModuleComment => "////",
            Token::EmptyLine => "EMPTYLINE",
            Token::As => "as",
            Token::Assert => "assert",
            Token::Check => "check",
            Token::When => "when",
            Token::Is => "is",
            Token::Const => "const",
            Token::Fn => "fn",
            Token::If => "if",
            Token::Else => "else",
            Token::Use => "import",
            Token::Let => "let",
            Token::Opaque => "opaque",
            Token::Pub => "pub",
            Token::Todo => "todo",
            Token::Trace => "try",
            Token::Type => "type",
            Token::Test => "test",
        };
        write!(f, "\"{}\"", s)
    }
}
