use std::fmt;

#[derive(Clone, Debug, PartialEq, Hash, Eq, Copy, serde::Serialize, serde::Deserialize)]
pub enum Base {
    Decimal { numeric_underscore: bool },
    Hexadecimal,
}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum Token {
    Error(char),
    Name { name: String },
    Ordinal { index: u32 },
    UpName { name: String },
    DiscardName { name: String },
    Int { value: String, base: Base },
    ByteString { value: String },
    String { value: String },
    // Groupings
    NewLineLeftParen, // ↳(
    LeftParen,        // (
    RightParen,       // )
    LeftSquare,       // [
    RightSquare,      // }
    LeftBrace,        // {
    RightBrace,       // }
    // Int Operators
    Plus,
    Minus,
    NewLineMinus,
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
    Hash,     // '#'
    Bang,     // '!'
    Question, // '?'
    Equal,
    EqualEqual,  // '=='
    NotEqual,    // '!='
    Vbar,        // '|'
    VbarVbar,    // '||'
    AmperAmper,  // '&&'
    And,         // and
    Or,          // or
    NewLinePipe, // '↳|>'
    Pipe,        // '|>'
    Dot,         // '.'
    RArrow,      // '->'
    LArrow,      // '<-'
    DotDot,      // '..'
    EndOfFile,
    // Docs/Extra
    Comment,
    DocComment,
    ModuleComment,
    EmptyLine,
    NewLine,
    // Keywords (alphabetically):
    As,
    Const,
    Fn,
    If,
    Else,
    Fail,
    Once,
    Expect,
    Is,
    Let,
    Opaque,
    Pub,
    Use,
    Test,
    Todo,
    Type,
    When,
    Trace,
    Validator,
    Via,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let index_str;
        let s = match self {
            Token::Error(c) => {
                write!(f, "\"{c}\"")?;
                return Ok(());
            }
            Token::Name { name } => name,
            Token::Ordinal { index } => {
                index_str = index.to_string();
                &index_str[..]
            }
            Token::UpName { name } => name,
            Token::DiscardName { name } => name,
            Token::Int { value, .. } => value,
            Token::String { value } => value,
            Token::ByteString { value } => value,
            Token::NewLineLeftParen => "↳(",
            Token::LeftParen => "(",
            Token::RightParen => ")",
            Token::LeftSquare => "[",
            Token::RightSquare => "]",
            Token::LeftBrace => "{",
            Token::RightBrace => "}",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::NewLineMinus => "↳-",
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
            Token::Question => "?",
            Token::EqualEqual => "==",
            Token::NotEqual => "!=",
            Token::Vbar => "|",
            Token::VbarVbar => "||",
            Token::AmperAmper => "&&",
            Token::And => "and",
            Token::Or => "or",
            Token::NewLinePipe => "↳|>",
            Token::Pipe => "|>",
            Token::Dot => ".",
            Token::RArrow => "->",
            Token::LArrow => "<-",
            Token::DotDot => "..",
            Token::EndOfFile => "EOF",
            Token::Comment => "//",
            Token::DocComment => "///",
            Token::ModuleComment => "////",
            Token::EmptyLine => "EMPTYLINE",
            Token::NewLine => "NEWLINE",
            Token::As => "as",
            Token::Expect => "expect",
            Token::When => "when",
            Token::Is => "is",
            Token::Const => "const",
            Token::Fn => "fn",
            Token::If => "if",
            Token::Else => "else",
            Token::Use => "use",
            Token::Let => "let",
            Token::Opaque => "opaque",
            Token::Pub => "pub",
            Token::Todo => "todo",
            Token::Trace => "trace",
            Token::Type => "type",
            Token::Test => "test",
            Token::Fail => "fail",
            Token::Once => "once",
            Token::Validator => "validator",
            Token::Via => "via",
        };
        write!(f, "{s}")
    }
}
