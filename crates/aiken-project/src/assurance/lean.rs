use aiken_lang::ast::Span;

/// UAL fragment id of an Aiken module: `/` becomes `.` and `.` becomes `-`.
///
/// Aiken module names use only `[a-z0-9_./]` (`is_aiken_path` admits dots after
/// the first segment and `-` is normalized to `_`), so this is injective and
/// always matches the fragment-id schema `^[A-Za-z0-9_.-]+$`. Ids of modules whose
/// path segments are plain identifiers are unchanged from the Lean spelling.
/// The id is a bundle key only; it is never a Lean name.
pub(crate) fn fragment_id(module: &str) -> String {
    module
        .chars()
        .map(|character| match character {
            '/' => '.',
            '.' => '-',
            other => other,
        })
        .collect()
}

/// Fragment namespaces preserve module path segments, escaping Lean syntax only.
pub(crate) fn namespace(module: &str) -> String {
    let mut result = String::with_capacity(module.len());
    for (index, segment) in module.split('/').enumerate() {
        if index != 0 {
            result.push('.');
        }
        push_segment(&mut result, segment);
    }
    result
}

/// Find lexically visible `def`/`abbrev` names, not necessarily valid Lean declarations.
/// `offset` is the absolute UTF-8 byte position of `body[0]` in the module source
/// (the parser's `body_location.start`, not its delimiter-inclusive block location).
/// Names are canonical Lean spellings; redundant `«…»` escapes are removed, but
/// escaped dots remain a single name segment. No module namespace is prepended.
/// Comments, string/character literals, and quoted name literals are not declarations.
pub(super) fn helpers(body: &str, offset: usize) -> Vec<(String, Span)> {
    let mut result = Vec::new();
    let mut declaration = false;
    for token in Lexer::new(body) {
        match token {
            Token::Name(location) => {
                let name = &body[location.start..location.end];
                if declaration && !keyword(name) {
                    let mut canonical = String::with_capacity(name.len());
                    for (index, segment) in Segments(name).enumerate() {
                        if index != 0 {
                            canonical.push('.');
                        }
                        push_segment(&mut canonical, segment);
                    }
                    result.push((
                        canonical,
                        Span {
                            start: offset + location.start,
                            end: offset + location.end,
                        },
                    ));
                }
                declaration = matches!(name, "def" | "abbrev");
            }
            Token::Other(_) => declaration = false,
        }
    }
    result
}

/// Match a namespace prefix of a qualified name, ignoring comments and literals.
/// Call this on parsed bodies so descriptions and annotation headers cannot match.
pub(super) fn references_namespace(body: &str, namespace: &str) -> bool {
    if namespace.is_empty() {
        return false;
    }
    Lexer::new(body).any(|token| {
        let Token::Name(location) = token else {
            return false;
        };
        let mut name = Segments(&body[location.start..location.end]);
        for segment in Segments(namespace) {
            if name.next() != Some(segment) {
                return false;
            }
        }
        name.next().is_some()
    })
}

/// Unescaped segments of a Lean name spelling; escaped dots stay in one segment.
pub(super) fn segments(name: &str) -> Vec<String> {
    Segments(name).map(str::to_owned).collect()
}

/// Canonical Lean spelling of unescaped name segments.
pub(super) fn spell(segments: &[String]) -> String {
    let mut result = String::new();
    for (index, segment) in segments.iter().enumerate() {
        if index != 0 {
            result.push('.');
        }
        push_segment(&mut result, segment);
    }
    result
}

/// The module-qualified copy of a predicate body: each `def`/`abbrev` name that
/// [`helpers`] finds gets the `namespace.` prefix; every other byte is unchanged.
/// Lean elaborates a dotted declaration with its prefixes opened, so unqualified
/// sibling references in the copy resolve to the qualified siblings.
pub(super) fn qualify(body: &str, namespace: &str) -> String {
    let mut result = String::with_capacity(body.len());
    let mut copied = 0;
    for (_, location) in helpers(body, 0) {
        result.push_str(&body[copied..location.start]);
        result.push_str(namespace);
        result.push('.');
        copied = location.start;
    }
    result.push_str(&body[copied..]);
    result
}

/// Whether a lexical name names `declaration` exactly, or projects from it.
/// Comments and literals never match. A local binder spelled like a module path
/// is indistinguishable here; callers must only use this for names that a
/// project helper declares, never for bare namespace prefixes.
pub(super) fn references_declaration(body: &str, declaration: &[String]) -> bool {
    Lexer::new(body).any(|token| {
        let Token::Name(location) = token else {
            return false;
        };
        let mut name = Segments(&body[location.start..location.end]);
        declaration
            .iter()
            .all(|segment| name.next() == Some(segment.as_str()))
    })
}

fn push_segment(result: &mut String, segment: &str) {
    let mut chars = segment.chars();
    if chars.next().is_some_and(id_first) && chars.all(id_rest) && !keyword(segment) {
        result.push_str(segment);
    } else {
        result.push('«');
        result.push_str(segment);
        result.push('»');
    }
}

// Lean 4.24's letter-like/subscript ranges, not Rust's broader Unicode alphabetic
// category: for example λ is syntax and an unescaped accented Latin letter is not
// a Lean identifier. See lean4 v4.24.0 src/util/name.cpp and Init/Meta/Defs.lean.
fn letter_like(char: char) -> bool {
    matches!(char,
        '\u{3b1}'..='\u{3c9}' | '\u{391}'..='\u{3a9}'
        | '\u{3ca}'..='\u{3fb}' | '\u{1f00}'..='\u{1ffe}'
        | '\u{2100}'..='\u{214f}' | '\u{1d49c}'..='\u{1d59f}'
    ) && !matches!(char, 'λ' | 'Π' | 'Σ')
}

fn id_first(char: char) -> bool {
    char.is_ascii_alphabetic() || char == '_' || letter_like(char)
}

fn id_rest(char: char) -> bool {
    id_first(char)
        || char.is_ascii_digit()
        || matches!(char, '\'' | '!' | '?' | '\u{207f}'..='\u{2089}'
            | '\u{2090}'..='\u{209c}' | '\u{1d62}'..='\u{1d6a}')
}

// Core Lean command/term keywords. Tactic names such as `simp` and `apply` are
// intentionally not reserved: Lean recognizes those through leading identifiers.
// Consumer-defined syntax is outside this producer-side lexical model.
fn keyword(name: &str) -> bool {
    matches!(
        name,
        "_" | "Prop"
            | "Sort"
            | "Type"
            | "abbrev"
            | "as"
            | "at"
            | "axiom"
            | "builtin_initialize"
            | "by"
            | "calc"
            | "class"
            | "decreasing_by"
            | "def"
            | "deriving"
            | "do"
            | "else"
            | "end"
            | "example"
            | "export"
            | "extends"
            | "for"
            | "forall"
            | "from"
            | "fun"
            | "have"
            | "if"
            | "import"
            | "in"
            | "include"
            | "inductive"
            | "infix"
            | "infixl"
            | "infixr"
            | "initialize"
            | "instance"
            | "let"
            | "macro"
            | "macro_rules"
            | "match"
            | "mutual"
            | "namespace"
            | "noncomputable"
            | "nomatch"
            | "notation"
            | "opaque"
            | "open"
            | "partial"
            | "postfix"
            | "prefix"
            | "prelude"
            | "private"
            | "protected"
            | "public"
            | "return"
            | "section"
            | "set_option"
            | "show"
            | "structure"
            | "syntax"
            | "then"
            | "theorem"
            | "universe"
            | "unsafe"
            | "variable"
            | "where"
            | "with"
            | "attribute"
            | "declare_syntax_cat"
            | "elab"
            | "elab_rules"
            | "local"
            | "scoped"
            | "omit"
            | "termination_by"
            | "termination_by?"
            | "finally"
            | "try"
            | "catch"
            | "unless"
            | "break"
            | "continue"
            | "while"
            | "mut"
            | "renaming"
            | "hiding"
            | "using"
            | "module"
            | "meta"
            | "all"
            | "nonrec"
            | "sorry"
            | "suffices"
            | "nofun"
            | "leading_parser"
            | "trailing_parser"
            | "let_fun"
            | "let_delayed"
            | "let_tmp"
            | "haveI"
            | "letI"
            | "partial_fixpoint"
            | "coinductive_fixpoint"
            | "inductive_fixpoint"
            | "no_index"
            | "panic!"
            | "unreachable!"
            | "dbg_trace"
            | "assert!"
            | "debug_assert!"
            | "StateRefT"
            | "show_term_elab"
            | "match_expr"
            | "let_expr"
            | "throwNamedError"
            | "throwNamedErrorAt"
            | "logNamedError"
            | "logNamedErrorAt"
            | "logNamedWarning"
            | "logNamedWarningAt"
            | "init_quot"
            | "add_decl_doc"
            | "register_tactic_tag"
            | "tactic_extension"
            | "recommended_spelling"
            | "register_error_explanation"
            | "register_parser_alias"
    )
}

/// Iterate name segments without splitting dots inside escaped identifiers.
struct Segments<'a>(&'a str);

impl<'a> Iterator for Segments<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0.is_empty() {
            return None;
        }
        if let Some(escaped) = self.0.strip_prefix('«') {
            let end = escaped.find('»')?;
            self.0 = escaped[end + '»'.len_utf8()..]
                .strip_prefix('.')
                .unwrap_or_default();
            Some(&escaped[..end])
        } else {
            let end = self.0.find('.').unwrap_or(self.0.len());
            let segment = &self.0[..end];
            self.0 = self.0[end..].strip_prefix('.').unwrap_or_default();
            Some(segment)
        }
    }
}

pub(super) enum Token {
    Name(Span),
    Other(Span),
}

/// Deliberately lexical: no Lean dependency, command elaboration, or body validation.
pub(super) struct Lexer<'a> {
    source: &'a str,
    position: usize,
}

impl<'a> Lexer<'a> {
    pub(super) fn new(source: &'a str) -> Self {
        Self {
            source,
            position: 0,
        }
    }

    fn remaining(&self) -> &'a str {
        &self.source[self.position..]
    }

    fn advance(&mut self) -> Option<char> {
        let char = self.remaining().chars().next()?;
        self.position += char.len_utf8();
        Some(char)
    }

    fn trivia(&mut self) {
        loop {
            self.position += self.remaining().len() - self.remaining().trim_start().len();
            if self.remaining().starts_with("--") {
                self.position += self
                    .remaining()
                    .find('\n')
                    .unwrap_or(self.remaining().len());
            } else if self.remaining().starts_with("/-") {
                self.position += 2;
                let mut depth = 1usize;
                while depth != 0 && !self.remaining().is_empty() {
                    if self.remaining().starts_with("/-") {
                        depth += 1;
                        self.position += 2;
                    } else if self.remaining().starts_with("-/") {
                        depth -= 1;
                        self.position += 2;
                    } else {
                        self.advance();
                    }
                }
            } else {
                return;
            }
        }
    }

    fn quoted(&mut self, delimiter: char) {
        self.advance();
        while let Some(char) = self.advance() {
            if char == '\\' {
                self.advance();
            } else if char == delimiter {
                break;
            }
        }
    }

    fn raw_string(&mut self) -> bool {
        let Some(after_r) = self.remaining().strip_prefix('r') else {
            return false;
        };
        let hashes = after_r.bytes().take_while(|byte| *byte == b'#').count();
        if !after_r[hashes..].starts_with('"') {
            return false;
        }
        self.position += 2 + hashes;
        while let Some(char) = self.advance() {
            if char == '"'
                && self
                    .remaining()
                    .as_bytes()
                    .get(..hashes)
                    .is_some_and(|bytes| bytes.iter().all(|byte| *byte == b'#'))
            {
                self.position += hashes;
                break;
            }
        }
        true
    }

    fn name_segment(&mut self) -> bool {
        if self.remaining().starts_with('«') {
            self.advance();
            if let Some(end) = self.remaining().find('»') {
                self.position += end + '»'.len_utf8();
                true
            } else {
                self.position = self.source.len();
                false
            }
        } else if self.remaining().chars().next().is_some_and(id_first) {
            self.advance();
            while self.remaining().chars().next().is_some_and(id_rest) {
                self.advance();
            }
            true
        } else {
            false
        }
    }

    fn name(&mut self) -> bool {
        if !self.name_segment() {
            return false;
        }
        while self.remaining().starts_with('.') {
            let before_dot = self.position;
            self.position += 1;
            if !self.name_segment() {
                self.position = before_dot;
                break;
            }
        }
        true
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.trivia();
        let start = self.position;
        let char = self.remaining().chars().next()?;
        if self.raw_string() {
            return Some(Token::Other(Span {
                start,
                end: self.position,
            }));
        }
        if matches!(char, '"' | '\'') {
            self.quoted(char);
            return Some(Token::Other(Span {
                start,
                end: self.position,
            }));
        }
        if char == '`' {
            self.advance();
            if self.remaining().starts_with('`') {
                self.advance();
            }
            self.name();
            return Some(Token::Other(Span {
                start,
                end: self.position,
            }));
        }
        if self.name() {
            return Some(Token::Name(Span {
                start,
                end: self.position,
            }));
        }
        self.advance();
        Some(Token::Other(Span {
            start,
            end: self.position,
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::{
        fragment_id, helpers, namespace, qualify, references_declaration, references_namespace,
        segments, spell,
    };

    #[test]
    fn fragment_ids_are_schema_legal_and_injective_over_aiken_module_names() {
        let modules = [
            "example",
            "namespace",
            "proofs/helpers",
            "a/b",
            "a.b",
            "a/b.c",
            "a.b/c",
            "a_b",
            "v2/x.1",
        ];
        let ids: Vec<_> = modules.iter().map(|module| fragment_id(module)).collect();
        assert_eq!(
            ids,
            [
                "example",
                "namespace",
                "proofs.helpers",
                "a.b",
                "a-b",
                "a.b-c",
                "a-b.c",
                "a_b",
                "v2.x-1"
            ]
        );
        assert_eq!(
            ids.iter().collect::<std::collections::BTreeSet<_>>().len(),
            modules.len()
        );
        for id in &ids {
            assert!(
                id.chars()
                    .all(|c| c.is_ascii_alphanumeric() || matches!(c, '_' | '.' | '-'))
            );
        }
        // Lean spellings of the colliding-looking pair stay distinct names.
        assert_eq!(namespace("a/b"), "a.b");
        assert_eq!(namespace("a.b"), "«a.b»");
        assert_ne!(segments(&namespace("a/b")), segments(&namespace("a.b")));
    }

    #[test]
    fn qualified_copies_prefix_only_declaration_names() {
        let body = concat!(
            "\n-- def commented := True\n",
            "def larger (old proposed : Int) : Prop := old < proposed\n",
            "abbrev «both» (a b c : Int) : Prop := larger a b ∧ larger b c -- def no\n",
            "def pick {α : Type} (x : α) : α := x\n",
            "def text := \"def quoted := True\"\n",
        );
        assert_eq!(
            qualify(body, "«example»"),
            concat!(
                "\n-- def commented := True\n",
                "def «example».larger (old proposed : Int) : Prop := old < proposed\n",
                "abbrev «example».«both» (a b c : Int) : Prop := larger a b ∧ larger b c -- def no\n",
                "def «example».pick {α : Type} (x : α) : α := x\n",
                "def «example».text := \"def quoted := True\"\n",
            )
        );
        assert_eq!(qualify("True", "a.b"), "True");
    }

    #[test]
    fn names_round_trip_through_segments() {
        for name in ["a.b", "«a.b».f", "«example».larger", "Some.«has.dot»", "α"] {
            assert_eq!(spell(&segments(name)), name);
        }
        assert_eq!(spell(&segments("«plain».x")), "plain.x");
    }

    #[test]
    fn declaration_references_need_the_whole_declared_name() {
        let declared = segments("ctx.unrelated");
        for body in [
            "ctx.unrelated",
            "∀ x, ctx.unrelated x",
            "ctx.unrelated.field",
            "«ctx».unrelated",
        ] {
            assert!(references_declaration(body, &declared), "{body}");
        }
        for body in [
            "∀ ctx : ScriptContext, ctx.scriptContextTxInfo = ctx.scriptContextTxInfo",
            "ctx",
            "ctx.unrelatedness",
            "other.ctx.unrelated",
            "-- ctx.unrelated",
            "\"ctx.unrelated\"",
            "«ctx.unrelated»",
        ] {
            assert!(!references_declaration(body, &declared), "{body}");
        }
    }

    #[test]
    fn namespaces_preserve_segments_and_escape_lean_syntax() {
        assert_eq!(namespace("vesting/onchain"), "vesting.onchain");
        assert_eq!(
            namespace("my-module/123name/has.dot"),
            "«my-module».«123name».«has.dot»"
        );
        assert_eq!(
            namespace("def/namespace/end/if/where/let"),
            "«def».«namespace».«end».«if».«where».«let»"
        );
        assert_eq!(
            namespace("ordinary_name/foo2/simp"),
            "ordinary_name.foo2.simp"
        );
        assert_eq!(namespace("α/λ/café/ℝ"), "α.«λ».«café».ℝ");
    }

    #[test]
    fn helpers_preserve_source_order_and_absolute_utf8_spans() {
        let body = "\n def α := True\n abbrev Some.«has.dot» := False\ndef «plain» := True\n";
        let offset = "// π before body\n".len();
        let names = helpers(body, offset);
        assert_eq!(
            names
                .iter()
                .map(|(name, _)| name.as_str())
                .collect::<Vec<_>>(),
            ["α", "Some.«has.dot»", "plain"]
        );
        for ((_, location), expected) in names.iter().zip(["α", "Some.«has.dot»", "«plain»"]) {
            assert_eq!(
                &body[location.start - offset..location.end - offset],
                expected
            );
        }
    }

    #[test]
    fn ignores_nested_comments_strings_characters_and_name_literals() {
        let body = concat!(
            "-- def line_comment := True\n",
            "/- abbrev outer := True /- def inner := True -/ def still_comment := True -/\n",
            "def visible := \"escaped \\\" def string_name := True\"\n",
            "abbrev /- between keyword and name -/ alias := visible\n",
            "def raw := r##\"literal \" def hidden := True \"# still raw\"##\n",
            "def character := '\"'\n",
            "def named := `def\n",
            "def named2 := ``abbrev\n",
            "abbrev last := True\n",
        );
        assert_eq!(
            helpers(body, 0)
                .into_iter()
                .map(|(name, _)| name)
                .collect::<Vec<_>>(),
            [
                "visible",
                "alias",
                "raw",
                "character",
                "named",
                "named2",
                "last"
            ],
        );
    }

    #[test]
    fn handles_escaped_qualified_names_without_conflating_escaped_dots() {
        let body = "def A.«same» := True\nabbrev «A».same := True\ndef «A.same» := False\ndef «def» := True";
        assert_eq!(
            helpers(body, 0)
                .into_iter()
                .map(|(name, _)| name)
                .collect::<Vec<_>>(),
            ["A.same", "A.same", "«A.same»", "«def»"],
        );
    }

    #[test]
    fn requires_keyword_tokens_and_immediately_following_names() {
        let body = "definition no; mydef no; A.def no; «def» no; def \"not a name\" no; abbrev := no; def actual := True";
        assert_eq!(
            helpers(body, 0)
                .into_iter()
                .map(|(name, _)| name)
                .collect::<Vec<_>>(),
            ["actual"]
        );
    }

    #[test]
    fn unterminated_comments_and_strings_hide_their_remainder() {
        assert!(helpers("/- def hidden := True", 0).is_empty());
        assert!(helpers("\"def hidden := True", 0).is_empty());
        assert!(helpers("r#\"def hidden := True", 0).is_empty());
    }

    #[test]
    fn namespace_references_are_qualified_names_not_text_matches() {
        assert!(references_namespace("∀ x, math.valid x", "math"));
        assert!(references_namespace("«math».valid", "math"));
        assert!(references_namespace("a.«with-dash».valid", "a.«with-dash»"));
        for body in [
            "-- math.valid\nTrue",
            "/- math.valid /- math.other -/ -/ True",
            "\"math.valid\"",
            "r#\"math.valid\"#",
            "`math.valid",
            "mathematics.valid",
            "other.math.valid",
            "math",
            "«math.valid»",
        ] {
            assert!(!references_namespace(body, "math"), "matched {body}");
        }
    }
}
