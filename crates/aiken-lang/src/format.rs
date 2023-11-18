use crate::{
    ast::{
        Annotation, Arg, ArgName, AssignmentKind, BinOp, ByteArrayFormatPreference, CallArg,
        ClauseGuard, Constant, CurveType, DataType, Definition, Function, IfBranch,
        LogicalOpChainKind, ModuleConstant, Pattern, RecordConstructor, RecordConstructorArg,
        RecordUpdateSpread, Span, TraceKind, TypeAlias, TypedArg, UnOp, UnqualifiedImport,
        UntypedArg, UntypedClause, UntypedClauseGuard, UntypedDefinition, UntypedFunction,
        UntypedModule, UntypedPattern, UntypedRecordUpdateArg, Use, Validator, CAPTURE_VARIABLE,
    },
    docvec,
    expr::{FnStyle, UntypedExpr, DEFAULT_ERROR_STR, DEFAULT_TODO_STR},
    parser::{
        extra::{Comment, ModuleExtra},
        token::Base,
    },
    pretty::{
        break_, concat, flex_break, join, line, lines, nil, prebreak, Document, Documentable,
    },
    tipo::{self, Type},
};
use itertools::Itertools;
use num_bigint::BigInt;
use ordinal::Ordinal;
use std::rc::Rc;
use vec1::Vec1;

const INDENT: isize = 2;
const DOCS_MAX_COLUMNS: isize = 80;

pub fn pretty(writer: &mut String, module: UntypedModule, extra: ModuleExtra, src: &str) {
    let intermediate = Intermediate {
        comments: extra
            .comments
            .iter()
            .map(|span| Comment::from((span, src)))
            .collect(),
        doc_comments: extra
            .doc_comments
            .iter()
            .map(|span| Comment::from((span, src)))
            .collect(),
        empty_lines: &extra.empty_lines,
        module_comments: extra
            .module_comments
            .iter()
            .map(|span| Comment::from((span, src)))
            .collect(),
    };

    Formatter::with_comments(&intermediate)
        .module(&module)
        .pretty_print(80, writer);
}

#[derive(Debug)]
struct Intermediate<'a> {
    comments: Vec<Comment<'a>>,
    doc_comments: Vec<Comment<'a>>,
    module_comments: Vec<Comment<'a>>,
    empty_lines: &'a [usize],
}

/// Hayleigh's bane
#[derive(Debug, Clone, Default)]
pub struct Formatter<'a> {
    comments: &'a [Comment<'a>],
    doc_comments: &'a [Comment<'a>],
    module_comments: &'a [Comment<'a>],
    empty_lines: &'a [usize],
}

impl<'comments> Formatter<'comments> {
    pub fn new() -> Self {
        Default::default()
    }

    fn with_comments(extra: &'comments Intermediate<'comments>) -> Self {
        Self {
            comments: &extra.comments,
            doc_comments: &extra.doc_comments,
            module_comments: &extra.module_comments,
            empty_lines: extra.empty_lines,
        }
    }

    // Pop comments that occur before a byte-index in the source, consuming
    // and retaining any empty lines contained within.
    fn pop_comments(&mut self, limit: usize) -> impl Iterator<Item = Option<&'comments str>> {
        let (popped, rest, empty_lines) =
            comments_before(self.comments, self.empty_lines, limit, true);

        self.comments = rest;

        self.empty_lines = empty_lines;

        popped
    }

    // Pop doc comments that occur before a byte-index in the source, consuming
    // and dropping any empty lines contained within.
    fn pop_doc_comments(&mut self, limit: usize) -> impl Iterator<Item = Option<&'comments str>> {
        let (popped, rest, empty_lines) =
            comments_before(self.doc_comments, self.empty_lines, limit, false);

        self.doc_comments = rest;

        self.empty_lines = empty_lines;

        popped
    }

    // Remove between 0 and `limit` empty lines following the current position,
    // returning true if any empty lines were removed.
    fn pop_empty_lines(&mut self, limit: usize) -> bool {
        let mut end = 0;

        for (i, &position) in self.empty_lines.iter().enumerate() {
            if position > limit {
                break;
            }
            end = i + 1;
        }

        self.empty_lines = self
            .empty_lines
            .get(end..)
            .expect("Pop empty lines slicing");

        end != 0
    }

    fn definitions<'a>(&mut self, definitions: &'a [UntypedDefinition]) -> Document<'a> {
        let mut has_imports = false;
        let mut has_declarations = false;
        let mut imports = Vec::new();
        let mut declarations = Vec::with_capacity(definitions.len());

        for def in definitions {
            let start = def.location().start;

            match def {
                Definition::Use(import) => {
                    has_imports = true;

                    let comments = self.pop_comments(start);

                    let def = self.definition(def);

                    imports.push((import, commented(def, comments)))
                }

                _other => {
                    has_declarations = true;

                    let comments = self.pop_comments(start);

                    let declaration = self.documented_definition(def);

                    declarations.push(commented(declaration, comments))
                }
            }
        }

        let imports = join(
            imports
                .into_iter()
                .sorted_by(|(import_a, _), (import_b, _)| {
                    Ord::cmp(&import_a.module, &import_b.module)
                })
                .map(|(_, doc)| doc),
            line(),
        );

        let declarations = join(declarations, lines(2));

        let sep = if has_imports && has_declarations {
            lines(2)
        } else {
            nil()
        };

        docvec![imports, sep, declarations]
    }

    fn module<'a>(&mut self, module: &'a UntypedModule) -> Document<'a> {
        let defs = self.definitions(&module.definitions);

        // Now that `defs` has been collected, only freestanding comments (//)
        // and doc comments (///) remain. Freestanding comments aren't associated
        // with any statement, and are moved to the bottom of the module.
        let doc_comments = join(
            self.doc_comments.iter().map(|comment| {
                "///"
                    .to_doc()
                    .append(Document::String(comment.content.to_string()))
            }),
            line(),
        );

        let comments = match printed_comments(self.pop_comments(usize::MAX), false) {
            Some(comments) => comments,
            None => nil(),
        };

        let module_comments = if !self.module_comments.is_empty() {
            let comments = self.module_comments.iter().map(|s| {
                "////"
                    .to_doc()
                    .append(Document::String(s.content.to_string()))
            });

            join(comments, line()).append(line())
        } else {
            nil()
        };

        let non_empty = vec![module_comments, defs, doc_comments, comments]
            .into_iter()
            .filter(|doc| !doc.is_empty());

        join(non_empty, line()).append(line())
    }

    fn definition<'a>(&mut self, definition: &'a UntypedDefinition) -> Document<'a> {
        match definition {
            Definition::Fn(Function {
                name,
                arguments: args,
                body,
                public,
                return_annotation,
                end_position,
                ..
            }) => self.definition_fn(
                public,
                "fn",
                name,
                args,
                return_annotation,
                body,
                *end_position,
                false,
            ),

            Definition::Validator(Validator {
                end_position,
                fun,
                other_fun,
                params,
                ..
            }) => self.definition_validator(params, fun, other_fun, *end_position),

            Definition::Test(Function {
                name,
                arguments: args,
                body,
                end_position,
                can_error,
                ..
            }) => self.definition_fn(
                &false,
                "test",
                name,
                args,
                &None,
                body,
                *end_position,
                *can_error,
            ),

            Definition::TypeAlias(TypeAlias {
                alias,
                parameters: args,
                annotation: resolved_type,
                public,
                ..
            }) => self.type_alias(*public, alias, args, resolved_type),

            Definition::DataType(DataType {
                name,
                parameters,
                public,
                constructors,
                location,
                opaque,
                ..
            }) => self.data_type(*public, *opaque, name, parameters, constructors, location),

            Definition::Use(import) => self.import(import),

            Definition::ModuleConstant(ModuleConstant {
                public,
                name,
                annotation,
                value,
                ..
            }) => {
                let head = pub_(*public).append("const ").append(name.as_str());
                let head = match annotation {
                    None => head,
                    Some(t) => head.append(": ").append(self.annotation(t)),
                };

                head.append(" =")
                    .append(break_("", " "))
                    .append(self.const_expr(value))
                    .nest(INDENT)
                    .group()
            }
        }
    }

    fn import<'a>(
        &mut self,
        Use {
            module,
            as_name,
            unqualified,
            ..
        }: &'a Use<()>,
    ) -> Document<'a> {
        "use "
            .to_doc()
            .append(Document::String(module.join("/")))
            .append(if unqualified.is_empty() {
                nil()
            } else {
                let unqualified = Itertools::intersperse(
                    unqualified
                        .iter()
                        .sorted_by(|a, b| a.name.cmp(&b.name))
                        .map(|e| e.to_doc()),
                    flex_break(",", ", "),
                );
                let unqualified = break_("", "")
                    .append(concat(unqualified))
                    .nest(INDENT)
                    .append(break_(",", ""))
                    .group();
                ".{".to_doc().append(unqualified).append("}")
            })
            .append(if let Some(name) = as_name {
                docvec![" as ", name]
            } else {
                nil()
            })
    }

    fn const_expr<'a>(&mut self, value: &'a Constant) -> Document<'a> {
        match value {
            Constant::ByteArray {
                bytes,
                preferred_format,
                ..
            } => self.bytearray(bytes, None, preferred_format),
            Constant::CurvePoint {
                point,
                preferred_format,
                ..
            } => self.bytearray(
                &point.compress(),
                Some(point.as_ref().into()),
                preferred_format,
            ),
            Constant::Int { value, base, .. } => self.int(value, base),
            Constant::String { value, .. } => self.string(value),
        }
    }

    pub fn docs_const_expr<'a>(&mut self, name: &'a str, value: &'a Constant) -> Document<'a> {
        let mut printer = tipo::pretty::Printer::new();
        name.to_doc()
            .append(": ")
            .append(printer.print(&value.tipo()))
            .append(" = ")
            .append(self.const_expr(value))
    }

    fn documented_definition<'a>(&mut self, s: &'a UntypedDefinition) -> Document<'a> {
        let comments = self.doc_comments(s.location().start);
        comments.append(self.definition(s).group()).group()
    }

    fn doc_comments<'a>(&mut self, limit: usize) -> Document<'a> {
        let mut comments = self.pop_doc_comments(limit).peekable();
        match comments.peek() {
            None => nil(),
            Some(_) => join(
                comments.map(|c| match c {
                    Some(c) => "///".to_doc().append(Document::String(c.to_string())),
                    None => unreachable!("empty lines dropped by pop_doc_comments"),
                }),
                line(),
            )
            .append(line())
            .force_break(),
        }
    }

    fn type_annotation_constructor<'a>(
        &mut self,
        module: &'a Option<String>,
        name: &'a str,
        args: &'a [Annotation],
    ) -> Document<'a> {
        let head = module
            .as_ref()
            .map(|qualifier| qualifier.to_doc().append(".").append(name))
            .unwrap_or_else(|| name.to_doc());

        if args.is_empty() {
            head
        } else {
            head.append(self.type_arguments(args))
        }
    }

    fn annotation<'a>(&mut self, t: &'a Annotation) -> Document<'a> {
        match t {
            Annotation::Hole { name, .. } => name.to_doc(),

            Annotation::Constructor {
                name,
                arguments: args,
                module,
                ..
            } => self.type_annotation_constructor(module, name, args),

            Annotation::Fn {
                arguments: args,
                ret: retrn,
                ..
            } => "fn"
                .to_doc()
                .append(wrap_args(args.iter().map(|t| (self.annotation(t), false))))
                .group()
                .append(" ->")
                .append(break_("", " ").append(self.annotation(retrn)).nest(INDENT)),

            Annotation::Var { name, .. } => name.to_doc(),
            Annotation::Tuple { elems, .. } => {
                wrap_args(elems.iter().map(|t| (self.annotation(t), false)))
            }
        }
        .group()
    }

    pub fn type_arguments<'a>(&mut self, args: &'a [Annotation]) -> Document<'a> {
        wrap_generics(args.iter().map(|t| self.annotation(t)))
    }

    pub fn type_alias<'a>(
        &mut self,
        public: bool,
        name: &'a str,
        args: &'a [String],
        typ: &'a Annotation,
    ) -> Document<'a> {
        let head = pub_(public).append("type ").append(name);

        let head = if args.is_empty() {
            head
        } else {
            head.append(wrap_generics(args.iter().map(|e| e.to_doc())).group())
        };

        head.append(" =")
            .append(line().append(self.annotation(typ)).group().nest(INDENT))
    }

    fn fn_arg<'a, A>(&mut self, arg: &'a Arg<A>) -> Document<'a> {
        let comments = self.pop_comments(arg.location.start);

        let doc_comments = self.doc_comments(arg.location.start);

        let doc = match &arg.annotation {
            None => arg.arg_name.to_doc(),
            Some(a) => arg
                .arg_name
                .to_doc()
                .append(": ")
                .append(self.annotation(a)),
        }
        .group();

        let doc = doc_comments.append(doc.group()).group();

        commented(doc, comments)
    }

    #[allow(clippy::too_many_arguments)]
    fn definition_fn<'a>(
        &mut self,
        public: &'a bool,
        keyword: &'a str,
        name: &'a str,
        args: &'a [UntypedArg],
        return_annotation: &'a Option<Annotation>,
        body: &'a UntypedExpr,
        end_location: usize,
        can_error: bool,
    ) -> Document<'a> {
        // Fn name and args
        let head = pub_(*public)
            .append(keyword)
            .append(" ")
            .append(name)
            .append(wrap_args(args.iter().map(|e| (self.fn_arg(e), false))))
            .append(if can_error { " fail" } else { "" });

        // Add return annotation
        let head = match return_annotation {
            Some(anno) => head.append(" -> ").append(self.annotation(anno)),
            None => head,
        }
        .group();

        // Format body
        let body = self.expr(body, true);

        // Add any trailing comments
        let body = match printed_comments(self.pop_comments(end_location), false) {
            Some(comments) => body.append(line()).append(comments),
            None => body,
        };

        // Stick it all together
        head.append(" {")
            .append(line().append(body).nest(INDENT).group())
            .append(line())
            .append("}")
    }

    fn definition_validator<'a>(
        &mut self,
        params: &'a [UntypedArg],
        fun: &'a UntypedFunction,
        other_fun: &'a Option<UntypedFunction>,
        end_position: usize,
    ) -> Document<'a> {
        // validator(params)
        let v_head = "validator".to_doc().append(if !params.is_empty() {
            wrap_args(params.iter().map(|e| (self.fn_arg(e), false)))
        } else {
            nil()
        });

        let fun_comments = self.pop_comments(fun.location.start);
        let fun_doc_comments = self.doc_comments(fun.location.start);
        let first_fn = self
            .definition_fn(
                &false,
                "fn",
                &fun.name,
                &fun.arguments,
                &fun.return_annotation,
                &fun.body,
                fun.end_position,
                false,
            )
            .group();
        let first_fn = commented(fun_doc_comments.append(first_fn).group(), fun_comments);

        let other_fn = match other_fun {
            None => nil(),
            Some(other) => {
                let other_comments = self.pop_comments(other.location.start);
                let other_doc_comments = self.doc_comments(other.location.start);

                let other_fn = self
                    .definition_fn(
                        &false,
                        "fn",
                        &other.name,
                        &other.arguments,
                        &other.return_annotation,
                        &other.body,
                        other.end_position,
                        false,
                    )
                    .group();

                commented(other_doc_comments.append(other_fn).group(), other_comments)
            }
        };

        let v_body = line()
            .append(first_fn)
            .append(if other_fun.is_some() { lines(2) } else { nil() })
            .append(other_fn);

        let v_body = match printed_comments(self.pop_comments(end_position), false) {
            Some(comments) => v_body.append(lines(2)).append(comments).nest(INDENT),
            None => v_body.nest(INDENT),
        };

        v_head
            .append(" {")
            .append(v_body)
            .append(line())
            .append("}")
    }

    fn expr_fn<'a>(
        &mut self,
        args: &'a [UntypedArg],
        return_annotation: Option<&'a Annotation>,
        body: &'a UntypedExpr,
    ) -> Document<'a> {
        let args = wrap_args(args.iter().map(|e| (self.fn_arg(e), false))).group();
        let body = match body {
            UntypedExpr::Trace { .. } | UntypedExpr::When { .. } => {
                self.expr(body, true).force_break()
            }
            _ => self.expr(body, true),
        };

        let header = "fn".to_doc().append(args);

        let header = match return_annotation {
            None => header,
            Some(t) => header.append(" -> ").append(self.annotation(t)),
        };

        header
            .append(
                break_(" {", " { ")
                    .append(body)
                    .nest(INDENT)
                    .append(break_("", " "))
                    .append("}"),
            )
            .group()
    }

    fn sequence<'a>(&mut self, expressions: &'a [UntypedExpr]) -> Document<'a> {
        let count = expressions.len();
        let mut documents = Vec::with_capacity(count * 2);

        for (i, expression) in expressions.iter().enumerate() {
            let preceding_newline = self.pop_empty_lines(expression.start_byte_index());

            if i != 0 && preceding_newline {
                documents.push(lines(2));
            } else if i != 0 {
                documents.push(lines(1));
            }

            documents.push(self.expr(expression, false).group());
        }

        documents.to_doc().force_break()
    }

    fn assignment<'a>(
        &mut self,
        pattern: &'a UntypedPattern,
        value: &'a UntypedExpr,
        kind: AssignmentKind,
        annotation: &'a Option<Annotation>,
    ) -> Document<'a> {
        let keyword = match kind {
            AssignmentKind::Let => "let",
            AssignmentKind::Expect => "expect",
        };

        match pattern {
            UntypedPattern::Constructor {
                name, module: None, ..
            } if name == "True" && annotation.is_none() && kind.is_expect() => {
                keyword.to_doc().append(self.case_clause_value(value))
            }
            _ => {
                self.pop_empty_lines(pattern.location().end);

                let pattern = self.pattern(pattern);

                let annotation = annotation
                    .as_ref()
                    .map(|a| ": ".to_doc().append(self.annotation(a)));

                keyword
                    .to_doc()
                    .append(" ")
                    .append(pattern.append(annotation).group())
                    .append(" =")
                    .append(self.case_clause_value(value))
            }
        }
    }

    pub fn bytearray<'a>(
        &mut self,
        bytes: &[u8],
        curve: Option<CurveType>,
        preferred_format: &ByteArrayFormatPreference,
    ) -> Document<'a> {
        match preferred_format {
            ByteArrayFormatPreference::HexadecimalString => "#"
                .to_doc()
                .append(Document::String(
                    curve.map(|c| c.to_string()).unwrap_or_default(),
                ))
                .append("\"")
                .append(Document::String(hex::encode(bytes)))
                .append("\""),
            ByteArrayFormatPreference::ArrayOfBytes(Base::Decimal { .. }) => "#"
                .to_doc()
                .append(Document::String(
                    curve.map(|c| c.to_string()).unwrap_or_default(),
                ))
                .append(
                    flex_break("[", "[")
                        .append(join(bytes.iter().map(|b| b.to_doc()), break_(",", ", ")))
                        .nest(INDENT)
                        .append(break_(",", ""))
                        .append("]"),
                )
                .group(),
            ByteArrayFormatPreference::ArrayOfBytes(Base::Hexadecimal) => "#"
                .to_doc()
                .append(Document::String(
                    curve.map(|c| c.to_string()).unwrap_or_default(),
                ))
                .append(
                    flex_break("[", "[")
                        .append(join(
                            bytes.iter().map(|b| {
                                Document::String(if *b < 16 {
                                    format!("0x0{b:x}")
                                } else {
                                    format!("{b:#x}")
                                })
                            }),
                            break_(",", ", "),
                        ))
                        .nest(INDENT)
                        .append(break_(",", ""))
                        .append("]"),
                )
                .group(),
            ByteArrayFormatPreference::Utf8String => nil()
                .append("\"")
                .append(Document::String(escape(
                    &String::from_utf8(bytes.to_vec()).unwrap(),
                )))
                .append("\""),
        }
    }

    pub fn int<'a>(&mut self, s: &'a str, base: &Base) -> Document<'a> {
        match s.chars().next() {
            Some('-') => Document::Str("-").append(self.uint(&s[1..], base)),
            _ => self.uint(s, base),
        }
    }

    pub fn uint<'a>(&mut self, s: &'a str, base: &Base) -> Document<'a> {
        match base {
            Base::Decimal { numeric_underscore } if *numeric_underscore => {
                let s = s
                    .chars()
                    .rev()
                    .enumerate()
                    .flat_map(|(i, c)| {
                        if i != 0 && i % 3 == 0 {
                            Some('_')
                        } else {
                            None
                        }
                        .into_iter()
                        .chain(std::iter::once(c))
                    })
                    .collect::<String>()
                    .chars()
                    .rev()
                    .collect::<String>();

                Document::String(s)
            }
            Base::Decimal { .. } => s.to_doc(),
            Base::Hexadecimal => Document::String(format!(
                "0x{}",
                BigInt::parse_bytes(s.as_bytes(), 10)
                    .expect("Invalid parsed hexadecimal digits ?!")
                    .to_str_radix(16),
            )),
        }
    }

    pub fn expr<'a>(&mut self, expr: &'a UntypedExpr, is_top_level: bool) -> Document<'a> {
        let comments = self.pop_comments(expr.start_byte_index());

        let document = match expr {
            UntypedExpr::ByteArray {
                bytes,
                preferred_format,
                ..
            } => self.bytearray(bytes, None, preferred_format),

            UntypedExpr::CurvePoint {
                point,
                preferred_format,
                ..
            } => self.bytearray(
                &point.compress(),
                Some(point.as_ref().into()),
                preferred_format,
            ),

            UntypedExpr::If {
                branches,
                final_else,
                ..
            } => self.if_expr(branches, final_else),

            UntypedExpr::LogicalOpChain {
                kind, expressions, ..
            } => self.logical_op_chain(kind, expressions),

            UntypedExpr::PipeLine {
                expressions,
                one_liner,
            } => self.pipeline(expressions, *one_liner),

            UntypedExpr::UInt { value, base, .. } => self.uint(value, base),

            UntypedExpr::String { value, .. } => self.string(value),

            UntypedExpr::Sequence { expressions, .. } => {
                let sequence = self.sequence(expressions);

                if is_top_level {
                    sequence
                } else {
                    "{".to_doc()
                        .append(line().append(sequence).nest(INDENT).group())
                        .append(line())
                        .append("}")
                }
            }

            UntypedExpr::Var { name, .. } if name.contains(CAPTURE_VARIABLE) => "_".to_doc(),

            UntypedExpr::Var { name, .. } => name.to_doc(),

            UntypedExpr::UnOp { value, op, .. } => self.un_op(value, op),

            UntypedExpr::Fn {
                fn_style: FnStyle::Capture,
                body,
                ..
            } => self.fn_capture(body),

            UntypedExpr::Fn {
                fn_style: FnStyle::BinOp(op),
                ..
            } => op.to_doc(),

            UntypedExpr::Fn {
                fn_style: FnStyle::Plain,
                return_annotation,
                arguments: args,
                body,
                ..
            } => self.expr_fn(args, return_annotation.as_ref(), body),

            UntypedExpr::List { elements, tail, .. } => self.list(elements, tail.as_deref()),

            UntypedExpr::Call {
                fun,
                arguments: args,
                ..
            } => self.call(fun, args),

            UntypedExpr::BinOp {
                name, left, right, ..
            } => self.bin_op(name, left, right),

            UntypedExpr::Assignment {
                value,
                pattern,
                annotation,
                kind,
                ..
            } => self.assignment(pattern, value, *kind, annotation),

            UntypedExpr::Trace {
                kind, text, then, ..
            } => self.trace(kind, text, then),

            UntypedExpr::When {
                subject, clauses, ..
            } => self.when(subject, clauses),

            UntypedExpr::FieldAccess {
                label, container, ..
            } => self
                .expr(container, false)
                .append(".")
                .append(label.as_str()),

            UntypedExpr::RecordUpdate {
                constructor,
                spread,
                arguments: args,
                ..
            } => self.record_update(constructor, spread, args),

            UntypedExpr::Tuple { elems, .. } => {
                wrap_args(elems.iter().map(|e| (self.wrap_expr(e), false))).group()
            }

            UntypedExpr::TupleIndex { index, tuple, .. } => {
                let suffix = Ordinal(*index + 1).suffix().to_doc();
                self.expr(tuple, false)
                    .append(".".to_doc())
                    .append((index + 1).to_doc())
                    .append(suffix)
            }

            UntypedExpr::ErrorTerm { .. } => "fail".to_doc(),

            UntypedExpr::TraceIfFalse { value, .. } => self.trace_if_false(value),
        };

        commented(document, comments)
    }

    fn string<'a>(&self, string: &'a str) -> Document<'a> {
        let doc = "@"
            .to_doc()
            .append(Document::String(escape(string)).surround("\"", "\""));
        if string.contains('\n') {
            doc.force_break()
        } else {
            doc
        }
    }

    pub fn trace_if_false<'a>(&mut self, value: &'a UntypedExpr) -> Document<'a> {
        docvec![self.wrap_unary_op(value), "?"]
    }

    pub fn trace<'a>(
        &mut self,
        kind: &'a TraceKind,
        text: &'a UntypedExpr,
        then: &'a UntypedExpr,
    ) -> Document<'a> {
        let (keyword, default_text) = match kind {
            TraceKind::Trace => ("trace", None),
            TraceKind::Error => ("fail", Some(DEFAULT_ERROR_STR.to_string())),
            TraceKind::Todo => ("todo", Some(DEFAULT_TODO_STR.to_string())),
        };

        let body = match text {
            UntypedExpr::String { value, .. } if Some(value) == default_text.as_ref() => {
                keyword.to_doc()
            }
            _ => keyword
                .to_doc()
                .append(" ")
                .append(self.wrap_expr(text))
                .group(),
        };

        match kind {
            TraceKind::Error | TraceKind::Todo => body,
            TraceKind::Trace => body
                .append(if self.pop_empty_lines(then.start_byte_index()) {
                    lines(2)
                } else {
                    line()
                })
                .append(self.expr(then, false)),
        }
    }

    pub fn pattern_constructor<'a>(
        &mut self,
        name: &'a str,
        args: &'a [CallArg<UntypedPattern>],
        module: &'a Option<String>,
        with_spread: bool,
        is_record: bool,
    ) -> Document<'a> {
        fn is_breakable(expr: &UntypedPattern) -> bool {
            match expr {
                Pattern::Tuple { .. } | Pattern::List { .. } => true,
                Pattern::Constructor {
                    arguments: args, ..
                } => !args.is_empty(),
                _ => false,
            }
        }

        let name = match module {
            Some(m) => m.to_doc().append(".").append(name),
            None => name.to_doc(),
        };

        if args.is_empty() && with_spread {
            if is_record {
                name.append(" { .. }")
            // TODO: not possible
            } else {
                name.append("(..)")
            }
        } else if args.is_empty() {
            name
        } else if with_spread {
            let wrapped_args = if is_record {
                wrap_fields_with_spread(args.iter().map(|a| self.pattern_call_arg(a)))
            } else {
                wrap_args_with_spread(args.iter().map(|a| self.pattern_call_arg(a)))
            };

            name.append(wrapped_args)
        } else {
            match args {
                [arg] if is_breakable(&arg.value) => name
                    .append(if is_record { "{" } else { "(" })
                    .append(self.pattern_call_arg(arg))
                    .append(if is_record { "}" } else { ")" })
                    .group(),

                _ => name
                    .append(wrap_args(
                        args.iter().map(|a| (self.pattern_call_arg(a), is_record)),
                    ))
                    .group(),
            }
        }
    }

    fn call<'a>(&mut self, fun: &'a UntypedExpr, args: &'a [CallArg<UntypedExpr>]) -> Document<'a> {
        let is_constr = match fun {
            UntypedExpr::Var { name, .. } => name[0..1].chars().all(|c| c.is_uppercase()),
            UntypedExpr::FieldAccess { label, .. } => label[0..1].chars().all(|c| c.is_uppercase()),
            _ => false,
        };

        let needs_curly = if is_constr {
            args.iter().all(|arg| arg.label.is_some())
        } else {
            false
        };

        self.expr(fun, false)
            .append(wrap_args(
                args.iter()
                    .map(|a| (self.call_arg(a, needs_curly), needs_curly)),
            ))
            .group()
    }

    pub fn if_expr<'a>(
        &mut self,
        branches: &'a Vec1<IfBranch<UntypedExpr>>,
        final_else: &'a UntypedExpr,
    ) -> Document<'a> {
        let if_branches = self
            .if_branch(break_("if", "if "), branches.first())
            .append(join(
                branches[1..].iter().map(|branch| {
                    self.if_branch(line().append(break_("} else if", "} else if ")), branch)
                }),
                nil(),
            ));

        let else_begin = line().append("} else {");

        let else_body = line().append(self.expr(final_else, true)).nest(INDENT);

        let else_end = line().append("}");

        if_branches
            .append(else_begin)
            .append(else_body)
            .append(else_end)
            .force_break()
    }

    pub fn if_branch<'a>(
        &mut self,
        if_keyword: Document<'a>,
        branch: &'a IfBranch<UntypedExpr>,
    ) -> Document<'a> {
        let if_begin = if_keyword
            .append(self.wrap_expr(&branch.condition))
            .append(break_("{", " {"))
            .group();

        let if_body = line().append(self.expr(&branch.body, true)).nest(INDENT);

        if_begin.append(if_body)
    }

    pub fn when<'a>(
        &mut self,
        subject: &'a UntypedExpr,
        clauses: &'a [UntypedClause],
    ) -> Document<'a> {
        let subjects_doc = break_("when", "when ")
            .append(self.wrap_expr(subject))
            .nest(INDENT)
            .append(break_("", " "))
            .append("is {")
            .group();

        let clauses_doc = concat(
            clauses
                .iter()
                .enumerate()
                .map(|(i, c)| self.clause(c, i as u32)),
        );

        subjects_doc
            .append(line().append(clauses_doc).nest(INDENT))
            .append(line())
            .append("}")
            .force_break()
    }

    pub fn record_update<'a>(
        &mut self,
        constructor: &'a UntypedExpr,
        spread: &'a RecordUpdateSpread,
        args: &'a [UntypedRecordUpdateArg],
    ) -> Document<'a> {
        use std::iter::once;
        let constructor_doc = self.expr(constructor, false);
        let spread_doc = "..".to_doc().append(self.expr(&spread.base, false));
        let arg_docs = args.iter().map(|a| (self.record_update_arg(a), true));
        let all_arg_docs = once((spread_doc, true)).chain(arg_docs);
        constructor_doc.append(wrap_args(all_arg_docs)).group()
    }

    pub fn bin_op<'a>(
        &mut self,
        name: &'a BinOp,
        left: &'a UntypedExpr,
        right: &'a UntypedExpr,
    ) -> Document<'a> {
        let precedence = name.precedence();

        let left_precedence = left.binop_precedence();
        let right_precedence = right.binop_precedence();

        let left = self.expr(left, false);
        let right = self.expr(right, false);

        self.operator_side(left, precedence, left_precedence)
            .append(" ")
            .append(name)
            .append(" ")
            .append(self.operator_side(right, precedence, right_precedence.saturating_sub(1)))
    }

    pub fn operator_side<'a>(&mut self, doc: Document<'a>, op: u8, side: u8) -> Document<'a> {
        if op > side {
            break_("(", "( ")
                .append(doc)
                .nest(INDENT)
                .append(break_("", " "))
                .append(")")
                .group()
        } else {
            doc
        }
    }

    fn logical_op_chain<'a>(
        &mut self,
        kind: &'a LogicalOpChainKind,
        expressions: &'a [UntypedExpr],
    ) -> Document<'a> {
        kind.to_doc()
            .append(" {")
            .append(
                line()
                    .append(join(
                        expressions
                            .iter()
                            .map(|expression| self.expr(expression, false)),
                        ",".to_doc().append(line()),
                    ))
                    .nest(INDENT)
                    .group(),
            )
            .append(",")
            .append(line())
            .append("}")
    }

    fn pipeline<'a>(
        &mut self,
        expressions: &'a Vec1<UntypedExpr>,
        one_liner: bool,
    ) -> Document<'a> {
        let mut docs = Vec::with_capacity(expressions.len() * 3);
        let first = expressions.first();
        let first_precedence = first.binop_precedence();
        let first = self.wrap_expr(first);
        docs.push(self.operator_side(first, 5, first_precedence));

        for expr in expressions.iter().skip(1) {
            let comments = self.pop_comments(expr.location().start);
            let doc = match expr {
                UntypedExpr::Fn {
                    fn_style: FnStyle::Capture,
                    body,
                    ..
                } => self.pipe_capture_right_hand_side(body),

                _ => self.wrap_expr(expr),
            };

            let expr = self
                .operator_side(doc, 4, expr.binop_precedence())
                .nest(2 * INDENT + 1);

            match printed_comments(comments, true) {
                None => {
                    let pipe = prebreak("|> ", " |> ").nest(INDENT);
                    docs.push(pipe.append(expr));
                }
                Some(comments) => {
                    let pipe = prebreak("|> ", "|> ");
                    docs.push(
                        " ".to_doc()
                            .append(comments.nest(INDENT).append(pipe.append(expr).group())),
                    );
                }
            }
        }

        if one_liner {
            docs.to_doc().group()
        } else {
            docs.to_doc().force_break()
        }
    }

    fn pipe_capture_right_hand_side<'a>(&mut self, fun: &'a UntypedExpr) -> Document<'a> {
        let (fun, args) = match fun {
            UntypedExpr::Call {
                fun,
                arguments: args,
                ..
            } => (fun, args),
            _ => panic!("Function capture found not to have a function call body when formatting"),
        };

        let hole_in_first_position = matches!(
            args.first(),
            Some(CallArg {
                value: UntypedExpr::Var { name, .. },
                ..
            }) if name.contains(CAPTURE_VARIABLE)
        );

        if hole_in_first_position && args.len() == 1 {
            // x |> fun(_)
            self.expr(fun, false)
        } else if hole_in_first_position {
            // x |> fun(_, 2, 3)
            self.expr(fun, false).append(
                wrap_args(
                    args.iter()
                        .skip(1)
                        .map(|a| (self.call_arg(a, false), false)),
                )
                .group(),
            )
        } else {
            // x |> fun(1, _, 3)
            self.expr(fun, false)
                .append(wrap_args(args.iter().map(|a| (self.call_arg(a, false), false))).group())
        }
    }

    fn fn_capture<'a>(&mut self, call: &'a UntypedExpr) -> Document<'a> {
        match call {
            UntypedExpr::Call {
                fun,
                arguments: args,
                ..
            } => match args.as_slice() {
                [first, second] if is_breakable_expr(&second.value) && first.is_capture_hole() => {
                    self.expr(fun, false)
                        .append("(_, ")
                        .append(self.call_arg(second, false))
                        .append(")")
                        .group()
                }

                _ => self.expr(fun, false).append(
                    wrap_args(args.iter().map(|a| (self.call_arg(a, false), false))).group(),
                ),
            },

            // The body of a capture being not a fn shouldn't be possible...
            _ => panic!("Function capture body found not to be a call in the formatter",),
        }
    }

    pub fn record_constructor<'a, A>(
        &mut self,
        constructor: &'a RecordConstructor<A>,
    ) -> Document<'a> {
        let comments = self.pop_comments(constructor.location.start);
        let doc_comments = self.doc_comments(constructor.location.start);

        let doc = if constructor.arguments.is_empty() {
            constructor.name.to_doc()
        } else if constructor.sugar {
            wrap_fields(constructor.arguments.iter().map(
                |RecordConstructorArg {
                     label,
                     annotation,
                     location,
                     ..
                 }| {
                    let arg_comments = self.pop_comments(location.start);

                    let arg = match label {
                        Some(l) => l.to_doc().append(": ").append(self.annotation(annotation)),
                        None => self.annotation(annotation),
                    };

                    commented(
                        self.doc_comments(location.start).append(arg).group(),
                        arg_comments,
                    )
                },
            ))
            .group()
        } else {
            constructor
                .name
                .to_doc()
                .append(wrap_args(constructor.arguments.iter().map(
                    |RecordConstructorArg {
                         label,
                         annotation,
                         location,
                         ..
                     }| {
                        let arg_comments = self.pop_comments(location.start);

                        let arg = match label {
                            Some(l) => l.to_doc().append(": ").append(self.annotation(annotation)),
                            None => self.annotation(annotation),
                        };

                        (
                            commented(
                                self.doc_comments(location.start).append(arg).group(),
                                arg_comments,
                            ),
                            label.is_some(),
                        )
                    },
                )))
                .group()
        };

        commented(doc_comments.append(doc).group(), comments)
    }

    pub fn data_type<'a, A>(
        &mut self,
        public: bool,
        opaque: bool,
        name: &'a str,
        args: &'a [String],
        constructors: &'a [RecordConstructor<A>],
        location: &'a Span,
    ) -> Document<'a> {
        self.pop_empty_lines(location.start);

        let mut is_sugar = false;

        pub_(public)
            .to_doc()
            .append(if opaque { "opaque type " } else { "type " })
            .append(if args.is_empty() {
                name.to_doc()
            } else {
                name.to_doc()
                    .append(wrap_generics(args.iter().map(|e| e.to_doc())))
                    .group()
            })
            .append(" {")
            .append(if constructors.len() == 1 && constructors[0].sugar {
                is_sugar = true;

                self.record_constructor(&constructors[0])
            } else {
                concat(constructors.iter().map(|c| {
                    if self.pop_empty_lines(c.location.start) {
                        lines(2)
                    } else {
                        line()
                    }
                    .append(self.record_constructor(c))
                    .nest(INDENT)
                    .group()
                }))
            })
            .append(if is_sugar { nil() } else { line() })
            .append("}")
    }

    pub fn docs_data_type<'a, A>(
        &mut self,
        name: &'a str,
        args: &'a [String],
        constructors: &'a [RecordConstructor<A>],
        location: &'a Span,
    ) -> Document<'a> {
        self.pop_empty_lines(location.start);

        let mut is_sugar = false;

        (if args.is_empty() {
            name.to_doc()
        } else {
            name.to_doc()
                .append(wrap_generics(args.iter().map(|e| e.to_doc())))
                .group()
        })
        .append(" {")
        .append(if constructors.len() == 1 && constructors[0].sugar {
            is_sugar = true;

            self.record_constructor(&constructors[0])
        } else {
            concat(constructors.iter().map(|c| {
                if self.pop_empty_lines(c.location.start) {
                    lines(2)
                } else {
                    line()
                }
                .append(self.record_constructor(c))
                .nest(INDENT)
                .group()
            }))
        })
        .append(if is_sugar { nil() } else { line() })
        .append("}")
    }

    pub fn docs_opaque_data_type<'a>(
        &mut self,
        name: &'a str,
        args: &'a [String],
        location: &'a Span,
    ) -> Document<'a> {
        self.pop_empty_lines(location.start);
        if args.is_empty() {
            name.to_doc()
        } else {
            name.to_doc()
                .append(wrap_generics(args.iter().map(|e| e.to_doc())).group())
        }
    }

    pub fn docs_type_alias<'a>(
        &mut self,
        name: &'a str,
        args: &'a [String],
        typ: &'a Annotation,
    ) -> Document<'a> {
        let head = name.to_doc();

        let head = if args.is_empty() {
            head
        } else {
            head.append(wrap_generics(args.iter().map(|e| e.to_doc())).group())
        };

        head.append(" = ")
            .append(self.annotation(typ).group().nest(INDENT))
    }

    pub fn docs_record_constructor<'a, A>(
        &mut self,
        constructor: &'a RecordConstructor<A>,
    ) -> Document<'a> {
        if constructor.arguments.is_empty() {
            constructor.name.to_doc()
        } else {
            constructor
                .name
                .to_doc()
                .append(wrap_args(constructor.arguments.iter().map(|arg| {
                    (
                        (match &arg.label {
                            Some(l) => l.to_doc().append(": "),
                            None => "".to_doc(),
                        })
                        .append(self.annotation(&arg.annotation)),
                        arg.label.is_some(),
                    )
                })))
                .group()
        }
    }

    pub fn docs_fn_signature<'a>(
        &mut self,
        name: &'a str,
        args: &'a [TypedArg],
        return_annotation: &'a Option<Annotation>,
        return_type: Rc<Type>,
    ) -> Document<'a> {
        let head = name.to_doc().append(self.docs_fn_args(args)).append(" -> ");

        let tail = self.type_or_annotation(return_annotation, &return_type);

        let doc = head.append(tail.clone()).group();

        // Wrap arguments on multi-lines if they are lengthy.
        if doc
            .clone()
            .to_pretty_string(DOCS_MAX_COLUMNS)
            .contains('\n')
        {
            let head = name
                .to_doc()
                .append(self.docs_fn_args(args).force_break())
                .append(" -> ");
            head.append(tail).group()
        } else {
            doc
        }
    }

    // Will always print the types, even if they were implicit in the original source
    pub fn docs_fn_args<'a>(&mut self, args: &'a [TypedArg]) -> Document<'a> {
        wrap_args(args.iter().map(|e| (self.docs_fn_arg(e), false)))
    }

    fn docs_fn_arg<'a>(&mut self, arg: &'a TypedArg) -> Document<'a> {
        self.docs_fn_arg_name(&arg.arg_name)
            .append(self.type_or_annotation(&arg.annotation, &arg.tipo))
            .group()
    }

    fn docs_fn_arg_name<'a>(&mut self, arg_name: &'a ArgName) -> Document<'a> {
        match arg_name {
            ArgName::Discarded { .. } => "".to_doc(),
            ArgName::Named { label, .. } => label.to_doc().append(": "),
        }
    }

    // Display type-annotation when available, or fallback to inferred type.
    fn type_or_annotation<'a>(
        &mut self,
        annotation: &'a Option<Annotation>,
        type_info: &Rc<Type>,
    ) -> Document<'a> {
        match annotation {
            Some(a) => self.annotation(a),
            None => tipo::pretty::Printer::new().print(type_info),
        }
    }

    fn wrap_expr<'a>(&mut self, expr: &'a UntypedExpr) -> Document<'a> {
        match expr {
            UntypedExpr::Trace {
                kind: TraceKind::Trace,
                ..
            }
            | UntypedExpr::Sequence { .. }
            | UntypedExpr::Assignment { .. } => "{"
                .to_doc()
                .append(line().append(self.expr(expr, true)).nest(INDENT))
                .append(line())
                .append("}")
                .force_break(),

            _ => self.expr(expr, false),
        }
    }

    fn call_arg<'a>(&mut self, arg: &'a CallArg<UntypedExpr>, can_pun: bool) -> Document<'a> {
        match &arg.label {
            Some(s) => {
                if can_pun && matches!(&arg.value, UntypedExpr::Var {  name, .. } if name == s) {
                    nil()
                } else {
                    commented(
                        s.to_doc().append(": "),
                        self.pop_comments(arg.location.start),
                    )
                }
            }
            None => nil(),
        }
        .append(self.wrap_expr(&arg.value))
    }

    fn record_update_arg<'a>(&mut self, arg: &'a UntypedRecordUpdateArg) -> Document<'a> {
        arg.label
            .to_doc()
            .append(": ")
            .append(self.wrap_expr(&arg.value))
    }

    fn case_clause_value<'a>(&mut self, expr: &'a UntypedExpr) -> Document<'a> {
        match expr {
            UntypedExpr::Trace {
                kind: TraceKind::Trace,
                ..
            }
            | UntypedExpr::Sequence { .. }
            | UntypedExpr::Assignment { .. } => " {"
                .to_doc()
                .append(line().append(self.expr(expr, true)).nest(INDENT).group())
                .append(line())
                .append("}")
                .force_break(),

            UntypedExpr::Fn { .. } | UntypedExpr::List { .. } => {
                line().append(self.expr(expr, false)).nest(INDENT).group()
            }

            UntypedExpr::When { .. } => line().append(self.expr(expr, false)).nest(INDENT).group(),

            _ => break_("", " ")
                .append(self.expr(expr, false))
                .nest(INDENT)
                .group(),
        }
    }

    fn clause<'a>(&mut self, clause: &'a UntypedClause, index: u32) -> Document<'a> {
        let space_before = self.pop_empty_lines(clause.location.start);
        let clause_doc = join(
            clause.patterns.iter().map(|p| self.pattern(p)),
            " | ".to_doc(),
        );
        let clause_doc = match &clause.guard {
            None => clause_doc,
            Some(guard) => clause_doc.append(" if ").append(self.clause_guard(guard)),
        };

        if index == 0 {
            clause_doc
        } else if space_before {
            lines(2).append(clause_doc)
        } else {
            lines(1).append(clause_doc)
        }
        .append(" ->")
        .append(self.case_clause_value(&clause.then))
    }

    fn list<'a>(
        &mut self,
        elements: &'a [UntypedExpr],
        tail: Option<&'a UntypedExpr>,
    ) -> Document<'a> {
        let comma: fn() -> Document<'a> =
            if tail.is_none() && elements.iter().all(UntypedExpr::is_simple_constant) {
                || flex_break(",", ", ")
            } else {
                || break_(",", ", ")
            };
        let elements_document = join(elements.iter().map(|e| self.wrap_expr(e)), comma());
        let tail = tail.map(|e| self.expr(e, false));
        list(elements_document, elements.len(), tail)
    }

    pub fn pattern<'a>(&mut self, pattern: &'a UntypedPattern) -> Document<'a> {
        let comments = self.pop_comments(pattern.location().start);
        let doc = match pattern {
            Pattern::Int { value, base, .. } => self.int(value, base),

            Pattern::Var { name, .. } => name.to_doc(),

            Pattern::Assign { name, pattern, .. } => {
                self.pattern(pattern).append(" as ").append(name.as_str())
            }

            Pattern::Discard { name, .. } => name.to_doc(),

            Pattern::Tuple { elems, .. } => {
                wrap_args(elems.iter().map(|e| (self.pattern(e), false))).group()
            }

            Pattern::List { elements, tail, .. } => {
                let elements_document =
                    join(elements.iter().map(|e| self.pattern(e)), break_(",", ", "));
                let tail = tail.as_ref().map(|e| {
                    if e.is_discard() {
                        nil()
                    } else {
                        self.pattern(e)
                    }
                });
                list(elements_document, elements.len(), tail)
            }

            Pattern::Constructor {
                name,
                arguments: args,
                module,
                with_spread,
                is_record,
                ..
            } => self.pattern_constructor(name, args, module, *with_spread, *is_record),
        };
        commented(doc, comments)
    }

    fn pattern_call_arg<'a>(&mut self, arg: &'a CallArg<UntypedPattern>) -> Document<'a> {
        if let (UntypedPattern::Var { name, .. }, Some(label)) = (&arg.value, &arg.label) {
            if name == label {
                return self.pattern(&arg.value);
            }
        }

        arg.label
            .as_ref()
            .map(|s| s.to_doc().append(": "))
            .unwrap_or_else(nil)
            .append(self.pattern(&arg.value))
    }

    pub fn clause_guard_bin_op<'a>(
        &mut self,
        name: &'a str,
        name_precedence: u8,
        left: &'a UntypedClauseGuard,
        right: &'a UntypedClauseGuard,
    ) -> Document<'a> {
        let left_precedence = left.precedence();
        let right_precedence = right.precedence();
        let left = self.clause_guard(left);
        let right = self.clause_guard(right);
        self.operator_side(left, name_precedence, left_precedence)
            .append(name)
            .append(self.operator_side(right, name_precedence, right_precedence.saturating_sub(1)))
    }

    fn clause_guard<'a>(&mut self, clause_guard: &'a UntypedClauseGuard) -> Document<'a> {
        match clause_guard {
            ClauseGuard::Not { value, .. } => {
                docvec!["!", self.clause_guard(value)]
            }
            ClauseGuard::And { left, right, .. } => {
                self.clause_guard_bin_op(" && ", clause_guard.precedence(), left, right)
            }
            ClauseGuard::Or { left, right, .. } => {
                self.clause_guard_bin_op(" || ", clause_guard.precedence(), left, right)
            }
            ClauseGuard::Equals { left, right, .. } => {
                self.clause_guard_bin_op(" == ", clause_guard.precedence(), left, right)
            }

            ClauseGuard::NotEquals { left, right, .. } => {
                self.clause_guard_bin_op(" != ", clause_guard.precedence(), left, right)
            }
            ClauseGuard::GtInt { left, right, .. } => {
                self.clause_guard_bin_op(" > ", clause_guard.precedence(), left, right)
            }

            ClauseGuard::GtEqInt { left, right, .. } => {
                self.clause_guard_bin_op(" >= ", clause_guard.precedence(), left, right)
            }
            ClauseGuard::LtInt { left, right, .. } => {
                self.clause_guard_bin_op(" < ", clause_guard.precedence(), left, right)
            }

            ClauseGuard::LtEqInt { left, right, .. } => {
                self.clause_guard_bin_op(" <= ", clause_guard.precedence(), left, right)
            }

            ClauseGuard::Var { name, .. } => name.to_doc(),

            ClauseGuard::Constant(constant) => self.const_expr(constant),
        }
    }

    fn un_op<'a>(&mut self, value: &'a UntypedExpr, op: &'a UnOp) -> Document<'a> {
        match op {
            UnOp::Not => docvec!["!", self.wrap_unary_op(value)],
            UnOp::Negate => docvec!["-", self.wrap_unary_op(value)],
        }
    }

    fn wrap_unary_op<'a>(&mut self, expr: &'a UntypedExpr) -> Document<'a> {
        match expr {
            UntypedExpr::BinOp { .. } => "(".to_doc().append(self.expr(expr, false)).append(")"),
            _ => self.wrap_expr(expr),
        }
    }
}

impl<'a> Documentable<'a> for &'a ArgName {
    fn to_doc(self) -> Document<'a> {
        match self {
            ArgName::Discarded { label, name, .. } | ArgName::Named { label, name, .. } => {
                if label == name {
                    name.to_doc()
                } else {
                    docvec![label, " ", name]
                }
            }
        }
    }
}

fn pub_(public: bool) -> Document<'static> {
    if public {
        "pub ".to_doc()
    } else {
        nil()
    }
}

impl<'a> Documentable<'a> for &'a UnqualifiedImport {
    fn to_doc(self) -> Document<'a> {
        self.name.to_doc().append(match &self.as_name {
            None => nil(),
            Some(s) => " as ".to_doc().append(s.as_str()),
        })
    }
}

impl<'a> Documentable<'a> for &'a LogicalOpChainKind {
    fn to_doc(self) -> Document<'a> {
        match self {
            LogicalOpChainKind::And => "and",
            LogicalOpChainKind::Or => "or",
        }
        .to_doc()
    }
}

impl<'a> Documentable<'a> for &'a BinOp {
    fn to_doc(self) -> Document<'a> {
        match self {
            BinOp::And => "&&",
            BinOp::Or => "||",
            BinOp::LtInt => "<",
            BinOp::LtEqInt => "<=",
            BinOp::Eq => "==",
            BinOp::NotEq => "!=",
            BinOp::GtEqInt => ">=",
            BinOp::GtInt => ">",
            BinOp::AddInt => "+",
            BinOp::SubInt => "-",
            BinOp::MultInt => "*",
            BinOp::DivInt => "/",
            BinOp::ModInt => "%",
        }
        .to_doc()
    }
}

pub fn wrap_args<'a, I>(args: I) -> Document<'a>
where
    I: IntoIterator<Item = (Document<'a>, bool)>,
{
    let mut args = args.into_iter().peekable();

    let curly = if let Some((_, uses_curly)) = args.peek() {
        *uses_curly
    } else {
        return "()".to_doc();
    };

    let args = args.map(|a| a.0);

    let (open_broken, open_unbroken, close) = if curly {
        (" {", " { ", "}")
    } else {
        ("(", "(", ")")
    };

    break_(open_broken, open_unbroken)
        .append(join(args, break_(",", ", ")))
        .nest(INDENT)
        .append(break_(",", if curly { " " } else { "" }))
        .append(close)
}

pub fn wrap_generics<'a, I>(args: I) -> Document<'a>
where
    I: IntoIterator<Item = Document<'a>>,
{
    break_("<", "<")
        .append(join(args, break_(",", ", ")))
        .nest(INDENT)
        .append(break_(",", ""))
        .append(">")
}

pub fn wrap_fields<'a, I>(args: I) -> Document<'a>
where
    I: IntoIterator<Item = Document<'a>>,
{
    let mut args = args.into_iter().peekable();
    if args.peek().is_none() {
        return nil();
    }

    line()
        .append(join(args, ",".to_doc().append(line())))
        .nest(INDENT)
        .append(",")
        .append(line())
}

pub fn wrap_args_with_spread<'a, I>(args: I) -> Document<'a>
where
    I: IntoIterator<Item = Document<'a>>,
{
    let mut args = args.into_iter().peekable();
    if args.peek().is_none() {
        return "()".to_doc();
    }

    break_("(", "(")
        .append(join(args, break_(",", ", ")))
        .append(break_(",", ", "))
        .append("..")
        .nest(INDENT)
        .append(break_(",", ""))
        .append(")")
        .group()
}

pub fn wrap_fields_with_spread<'a, I>(args: I) -> Document<'a>
where
    I: IntoIterator<Item = Document<'a>>,
{
    let mut args = args.into_iter().peekable();
    if args.peek().is_none() {
        return "()".to_doc();
    }

    break_(" {", " { ")
        .append(join(args, break_(",", ", ")))
        .append(break_(",", ", "))
        .append("..")
        .nest(INDENT)
        .append(break_("", " "))
        .append("}")
        .group()
}

fn list<'a>(elements: Document<'a>, length: usize, tail: Option<Document<'a>>) -> Document<'a> {
    if length == 0 {
        return match tail {
            Some(tail) => tail,
            None => "[]".to_doc(),
        };
    }

    let doc = break_("[", "[").append(elements);

    match tail {
        None => doc.nest(INDENT).append(break_(",", "")),

        // Don't print tail if it is a discard
        Some(Document::String(t)) if t == *"_" => doc
            .append(break_(",", ", "))
            .append("..")
            .nest(INDENT)
            .append(break_("", "")),

        Some(final_tail) => doc
            .append(break_(",", ", "))
            .append("..")
            .append(final_tail)
            .nest(INDENT)
            .append(break_("", "")),
    }
    .append("]")
    .group()
}

fn printed_comments<'a, 'comments>(
    comments: impl IntoIterator<Item = Option<&'comments str>>,
    trailing_newline: bool,
) -> Option<Document<'a>> {
    let mut comments = comments.into_iter().peekable();
    comments.peek()?;

    let mut doc = Vec::new();
    while let Some(c) = comments.next() {
        match c {
            None => continue,
            Some(c) => {
                // There will never be consecutive empty lines (None values),
                // and whenever we peek a None, we advance past it.
                doc.push("//".to_doc().append(Document::String(c.to_string())));
                match comments.peek() {
                    // Next line is a comment
                    Some(Some(_)) => doc.push(line()),
                    // Next line is empty
                    Some(None) => {
                        comments.next();
                        match comments.peek() {
                            Some(_) => doc.push(lines(2)),
                            None => {
                                if trailing_newline {
                                    doc.push(lines(2));
                                }
                            }
                        }
                    }
                    // We've reached the end, there are no more lines
                    None => {
                        if trailing_newline {
                            doc.push(line());
                        }
                    }
                }
            }
        }
    }
    let doc = concat(doc);
    if trailing_newline {
        Some(doc.force_break())
    } else {
        Some(doc)
    }
}

fn commented<'a, 'comments>(
    doc: Document<'a>,
    comments: impl IntoIterator<Item = Option<&'comments str>>,
) -> Document<'a> {
    match printed_comments(comments, true) {
        Some(comments) => comments.append(doc.group()),
        None => doc,
    }
}

pub fn comments_before<'a>(
    comments: &'a [Comment<'a>],
    empty_lines: &'a [usize],
    limit: usize,
    retain_empty_lines: bool,
) -> (
    impl Iterator<Item = Option<&'a str>>,
    &'a [Comment<'a>],
    &'a [usize],
) {
    let end_comments = comments
        .iter()
        .position(|c| c.start > limit)
        .unwrap_or(comments.len());
    let end_empty_lines = empty_lines
        .iter()
        .position(|l| *l > limit)
        .unwrap_or(empty_lines.len());
    let popped_comments = comments
        .get(0..end_comments)
        .expect("0..end_comments is guaranteed to be in bounds")
        .iter()
        .map(|c| (c.start, Some(c.content)));
    let popped_empty_lines = if retain_empty_lines { empty_lines } else { &[] }
        .get(0..end_empty_lines)
        .unwrap_or(&[])
        .iter()
        .map(|i| (i, i))
        // compact consecutive empty lines into a single line
        .coalesce(|(a_start, a_end), (b_start, b_end)| {
            if *a_end + 1 == *b_start {
                Ok((a_start, b_end))
            } else {
                Err(((a_start, a_end), (b_start, b_end)))
            }
        })
        .map(|l| (*l.0, None));
    let popped = popped_comments
        .merge_by(popped_empty_lines, |(a, _), (b, _)| a < b)
        .skip_while(|(_, comment_or_line)| comment_or_line.is_none())
        .map(|(_, comment_or_line)| comment_or_line);
    (
        popped,
        comments.get(end_comments..).expect("in bounds"),
        empty_lines.get(end_empty_lines..).expect("in bounds"),
    )
}

fn is_breakable_expr(expr: &UntypedExpr) -> bool {
    matches!(
        expr,
        UntypedExpr::Fn { .. }
            | UntypedExpr::Sequence { .. }
            | UntypedExpr::Assignment { .. }
            | UntypedExpr::Call { .. }
            | UntypedExpr::When { .. }
            | UntypedExpr::List { .. }
            | UntypedExpr::If { .. }
    )
}

fn escape(string: &str) -> String {
    string
        .chars()
        .flat_map(|c| match c {
            '\n' => vec!['\\', 'n'],
            '\r' => vec!['\\', 'r'],
            '\t' => vec!['\\', 't'],
            '"' => vec!['\\', c],
            '\\' => vec!['\\', c],
            _ => vec![c],
        })
        .collect::<String>()
}
