use super::{Type, TypeVar};
use crate::{
    docvec, format,
    pretty::{nil, *},
    tipo::{Annotation, TypeAliasAnnotation},
};
use itertools::Itertools;
use std::{collections::HashMap, rc::Rc};

const INDENT: isize = 2;

// TODO: use references instead of cloning strings and vectors
#[derive(Debug, Default)]
pub struct Printer {
    names: HashMap<u64, String>,
    uid: u64,
    // A mapping of printd type names to the module that they are defined in.
    printed_types: HashMap<String, String>,
}

impl Printer {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn with_names(&mut self, names: HashMap<u64, String>) {
        self.names = names;
    }

    /// Render a Type as a well formatted string.
    ///
    pub fn pretty_print(&mut self, typ: &Type, initial_indent: usize) -> String {
        let mut buffer = String::with_capacity(initial_indent);

        for _ in 0..initial_indent {
            buffer.push(' ');
        }

        buffer
            .to_doc()
            .append(self.print(typ))
            .nest(initial_indent as isize)
            .to_pretty_string(format::MAX_COLUMNS)
    }

    // TODO: have this function return a Document that borrows from the Type.
    // Is this possible? The lifetime would have to go through the Rc<Refcell<Type>>
    // for TypeVar::Link'd types.
    pub fn print<'a>(&mut self, typ: &Type) -> Document<'a> {
        if let Some(TypeAliasAnnotation {
            alias,
            parameters,
            annotation,
        }) = typ.alias().as_deref()
        {
            if let Some(resolved_parameters) = resolve_alias(parameters, annotation, typ) {
                return self.type_alias_doc(typ, alias.to_string(), resolved_parameters);
            }
        }

        match typ {
            Type::App {
                name, args, module, ..
            } => {
                let doc = if self.name_clashes_if_unqualified(name, module) {
                    qualify_type_name(module, name)
                } else {
                    self.printed_types.insert(name.clone(), module.clone());
                    Document::String(name.clone())
                };
                if args.is_empty() {
                    doc
                } else {
                    doc.append("<")
                        .append(self.args_to_aiken_doc(args))
                        .append(">")
                }
            }

            Type::Fn { args, ret, .. } => "fn("
                .to_doc()
                .append(self.args_to_aiken_doc(args))
                .append(") ->")
                .append(break_("", " ").append(self.print(ret)).nest(INDENT).group()),

            Type::Var { tipo: typ, .. } => self.type_var_doc(&typ.borrow()),

            Type::Tuple { elems, .. } => self.args_to_aiken_doc(elems).surround("(", ")"),
            Type::Pair { fst, snd, .. } => self
                .args_to_aiken_doc(&[fst.clone(), snd.clone()])
                .surround("Pair<", ">"),
        }
    }

    fn type_alias_doc<'a>(
        &mut self,
        typ: &Type,
        alias: String,
        parameters: Vec<Rc<Type>>,
    ) -> Document<'a> {
        let doc = Document::String(alias);

        if !parameters.is_empty() {
            doc.append(
                break_("", "")
                    .append(concat(Itertools::intersperse(
                        parameters.iter().map(|t| {
                            // Avoid infinite recursion for recursive types instantiated to
                            // themselves. For example: type Identity<t> = t
                            if t.as_ref() == typ {
                                self.print(typ.clone().set_alias(None).as_ref())
                            } else {
                                self.print(t)
                            }
                        }),
                        break_(",", ", "),
                    )))
                    .nest(INDENT)
                    .append(break_(",", ""))
                    .group()
                    .surround("<", ">"),
            )
        } else {
            doc
        }
    }

    fn name_clashes_if_unqualified(&mut self, tipo: &String, module: &String) -> bool {
        match self.printed_types.get(tipo) {
            None => false,
            Some(previous_module) if module == previous_module => false,
            Some(_different_module) => true,
        }
    }

    fn type_var_doc<'a>(&mut self, typ: &TypeVar) -> Document<'a> {
        match typ {
            TypeVar::Link { tipo: ref typ, .. } => self.print(typ),
            TypeVar::Generic { id, .. } => self.generic_type_var(*id),
            TypeVar::Unbound { .. } => "?".to_doc(),
        }
    }

    pub fn generic_type_var<'a>(&mut self, id: u64) -> Document<'a> {
        match self.names.get(&id) {
            Some(n) => {
                let typ_name = n.clone();

                self.printed_types.insert(typ_name, "".to_string());

                Document::String(n.clone())
            }
            None => {
                let n = self.next_letter();

                self.names.insert(id, n.clone());

                self.printed_types.insert(n.clone(), "".to_string());

                Document::String(n)
            }
        }
    }

    fn next_letter(&mut self) -> String {
        let alphabet_length = 26;
        let char_offset = 97;
        let mut chars = vec![];
        let mut n;
        let mut rest = self.uid;

        loop {
            n = rest % alphabet_length;

            rest /= alphabet_length;

            chars.push((n as u8 + char_offset) as char);

            if rest == 0 {
                break;
            }

            rest -= 1
        }

        self.uid += 1;

        chars.into_iter().rev().collect()
    }

    fn args_to_aiken_doc<'a>(&mut self, args: &[Rc<Type>]) -> Document<'a> {
        if args.is_empty() {
            return nil();
        }

        let args = concat(Itertools::intersperse(
            args.iter().map(|t| self.print(t).group()),
            break_(",", ", "),
        ));

        break_("", "")
            .append(args)
            .nest(INDENT)
            .append(break_(",", ""))
            .group()
    }
}

fn qualify_type_name(module: &str, typ_name: &str) -> Document<'static> {
    if module.is_empty() {
        docvec!["aiken.", Document::String(typ_name.to_string())]
    } else {
        Document::String([module, typ_name].join("."))
    }
}

pub fn resolve_alias(
    parameters: &[String],
    annotation: &Annotation,
    typ: &Type,
) -> Option<Vec<Rc<Type>>> {
    let mut types = Vec::new();

    fn resolve_one(parameter: &str, annotation: &Annotation, typ: Rc<Type>) -> Option<Rc<Type>> {
        match (annotation, typ.as_ref()) {
            (
                Annotation::Fn {
                    arguments: args,
                    ret,
                    ..
                },
                Type::Fn {
                    args: t_args,
                    ret: t_ret,
                    ..
                },
            ) => {
                let mut result = resolve_one(parameter, ret, t_ret.clone());
                for (ann, t) in args.iter().zip(t_args) {
                    result = result.or_else(|| resolve_one(parameter, ann, t.clone()));
                }
                result
            }

            (
                Annotation::Constructor {
                    arguments: args, ..
                },
                Type::App { args: t_args, .. },
            ) => {
                let mut result = None;
                for (ann, t) in args.iter().zip(t_args) {
                    result = result.or_else(|| resolve_one(parameter, ann, t.clone()));
                }
                result
            }

            (Annotation::Tuple { elems, .. }, Type::Tuple { elems: t_elems, .. }) => {
                let mut result = None;
                for (ann, t) in elems.iter().zip(t_elems) {
                    result = result.or_else(|| resolve_one(parameter, ann, t.clone()));
                }
                result
            }

            (
                Annotation::Pair { fst, snd, .. },
                Type::Pair {
                    fst: t_fst,
                    snd: t_snd,
                    ..
                },
            ) => {
                let mut result = None;
                for (ann, t) in [fst, snd].into_iter().zip([t_fst, t_snd]) {
                    result = result.or_else(|| resolve_one(parameter, ann, t.clone()));
                }
                result
            }

            (Annotation::Var { name, .. }, ..) if name == parameter => Some(typ),

            _ => None,
        }
    }

    let rc: Rc<Type> = typ.to_owned().into();

    for parameter in parameters {
        types.push(resolve_one(parameter, annotation, rc.clone())?);
    }

    Some(types)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tipo::{Span, Type};
    use pretty_assertions::assert_eq;
    use std::cell::RefCell;

    #[test]
    fn next_letter_test() {
        let mut printer = Printer::new();
        assert_eq!(printer.next_letter(), "a".to_string());
        assert_eq!(printer.next_letter(), "b".to_string());
        assert_eq!(printer.next_letter(), "c".to_string());
        assert_eq!(printer.next_letter(), "d".to_string());
        assert_eq!(printer.next_letter(), "e".to_string());
        assert_eq!(printer.next_letter(), "f".to_string());
        assert_eq!(printer.next_letter(), "g".to_string());
        assert_eq!(printer.next_letter(), "h".to_string());
        assert_eq!(printer.next_letter(), "i".to_string());
        assert_eq!(printer.next_letter(), "j".to_string());
        assert_eq!(printer.next_letter(), "k".to_string());
        assert_eq!(printer.next_letter(), "l".to_string());
        assert_eq!(printer.next_letter(), "m".to_string());
        assert_eq!(printer.next_letter(), "n".to_string());
        assert_eq!(printer.next_letter(), "o".to_string());
        assert_eq!(printer.next_letter(), "p".to_string());
        assert_eq!(printer.next_letter(), "q".to_string());
        assert_eq!(printer.next_letter(), "r".to_string());
        assert_eq!(printer.next_letter(), "s".to_string());
        assert_eq!(printer.next_letter(), "t".to_string());
        assert_eq!(printer.next_letter(), "u".to_string());
        assert_eq!(printer.next_letter(), "v".to_string());
        assert_eq!(printer.next_letter(), "w".to_string());
        assert_eq!(printer.next_letter(), "x".to_string());
        assert_eq!(printer.next_letter(), "y".to_string());
        assert_eq!(printer.next_letter(), "z".to_string());
        assert_eq!(printer.next_letter(), "aa".to_string());
        assert_eq!(printer.next_letter(), "ab".to_string());
        assert_eq!(printer.next_letter(), "ac".to_string());
        assert_eq!(printer.next_letter(), "ad".to_string());
        assert_eq!(printer.next_letter(), "ae".to_string());
        assert_eq!(printer.next_letter(), "af".to_string());
        assert_eq!(printer.next_letter(), "ag".to_string());
        assert_eq!(printer.next_letter(), "ah".to_string());
        assert_eq!(printer.next_letter(), "ai".to_string());
        assert_eq!(printer.next_letter(), "aj".to_string());
        assert_eq!(printer.next_letter(), "ak".to_string());
        assert_eq!(printer.next_letter(), "al".to_string());
        assert_eq!(printer.next_letter(), "am".to_string());
        assert_eq!(printer.next_letter(), "an".to_string());
        assert_eq!(printer.next_letter(), "ao".to_string());
        assert_eq!(printer.next_letter(), "ap".to_string());
        assert_eq!(printer.next_letter(), "aq".to_string());
        assert_eq!(printer.next_letter(), "ar".to_string());
        assert_eq!(printer.next_letter(), "as".to_string());
        assert_eq!(printer.next_letter(), "at".to_string());
        assert_eq!(printer.next_letter(), "au".to_string());
        assert_eq!(printer.next_letter(), "av".to_string());
        assert_eq!(printer.next_letter(), "aw".to_string());
        assert_eq!(printer.next_letter(), "ax".to_string());
        assert_eq!(printer.next_letter(), "ay".to_string());
        assert_eq!(printer.next_letter(), "az".to_string());
        assert_eq!(printer.next_letter(), "ba".to_string());
        assert_eq!(printer.next_letter(), "bb".to_string());
        assert_eq!(printer.next_letter(), "bc".to_string());
        assert_eq!(printer.next_letter(), "bd".to_string());
        assert_eq!(printer.next_letter(), "be".to_string());
        assert_eq!(printer.next_letter(), "bf".to_string());
        assert_eq!(printer.next_letter(), "bg".to_string());
        assert_eq!(printer.next_letter(), "bh".to_string());
        assert_eq!(printer.next_letter(), "bi".to_string());
        assert_eq!(printer.next_letter(), "bj".to_string());
        assert_eq!(printer.next_letter(), "bk".to_string());
        assert_eq!(printer.next_letter(), "bl".to_string());
        assert_eq!(printer.next_letter(), "bm".to_string());
        assert_eq!(printer.next_letter(), "bn".to_string());
        assert_eq!(printer.next_letter(), "bo".to_string());
        assert_eq!(printer.next_letter(), "bp".to_string());
        assert_eq!(printer.next_letter(), "bq".to_string());
        assert_eq!(printer.next_letter(), "br".to_string());
        assert_eq!(printer.next_letter(), "bs".to_string());
        assert_eq!(printer.next_letter(), "bt".to_string());
        assert_eq!(printer.next_letter(), "bu".to_string());
        assert_eq!(printer.next_letter(), "bv".to_string());
        assert_eq!(printer.next_letter(), "bw".to_string());
        assert_eq!(printer.next_letter(), "bx".to_string());
        assert_eq!(printer.next_letter(), "by".to_string());
        assert_eq!(printer.next_letter(), "bz".to_string());
    }

    #[test]
    fn pretty_print_test() {
        macro_rules! assert_string {
            ($src:expr, $typ:expr $(,)?) => {
                let mut printer = Printer::new();
                assert_eq!($typ.to_string(), printer.pretty_print(&$src, 0),);
            };
        }

        assert_string!(
            Type::App {
                module: "whatever".to_string(),
                name: "Int".to_string(),
                public: true,
                contains_opaque: false,
                args: vec![],
                alias: None
            },
            "Int",
        );
        assert_string!(
            Type::App {
                module: "".to_string(),
                name: "Pair".to_string(),
                public: true,
                contains_opaque: false,
                alias: None,
                args: vec![
                    Rc::new(Type::App {
                        module: "whatever".to_string(),
                        name: "Int".to_string(),
                        public: true,
                        contains_opaque: false,
                        args: vec![],
                        alias: None
                    }),
                    Rc::new(Type::App {
                        module: "whatever".to_string(),
                        name: "Bool".to_string(),
                        public: true,
                        contains_opaque: false,
                        args: vec![],
                        alias: None
                    }),
                ],
            },
            "Pair<Int, Bool>",
        );
        assert_string!(
            Type::Fn {
                args: vec![
                    Rc::new(Type::App {
                        args: vec![],
                        module: "whatever".to_string(),
                        name: "Int".to_string(),
                        public: true,
                        contains_opaque: false,
                        alias: None,
                    }),
                    Rc::new(Type::App {
                        args: vec![],
                        module: "whatever".to_string(),
                        name: "Bool".to_string(),
                        public: true,
                        contains_opaque: false,
                        alias: None,
                    }),
                ],
                ret: Rc::new(Type::App {
                    args: vec![],
                    module: "whatever".to_string(),
                    name: "Bool".to_string(),
                    public: true,
                    contains_opaque: false,
                    alias: None,
                }),
                alias: None,
            },
            "fn(Int, Bool) -> Bool",
        );
        assert_string!(
            Type::Var {
                alias: None,
                tipo: Rc::new(RefCell::new(TypeVar::Link {
                    tipo: Rc::new(Type::App {
                        alias: None,
                        args: vec![],
                        module: "whatever".to_string(),
                        name: "Int".to_string(),
                        public: true,
                        contains_opaque: false,
                    }),
                })),
            },
            "Int",
        );
        assert_string!(
            Type::Var {
                tipo: Rc::new(RefCell::new(TypeVar::Unbound { id: 2231 })),
                alias: None,
            },
            "?",
        );
        assert_string!(
            Type::function(
                vec![Rc::new(Type::Var {
                    tipo: Rc::new(RefCell::new(TypeVar::Unbound { id: 78 })),
                    alias: None,
                })],
                Rc::new(Type::Var {
                    tipo: Rc::new(RefCell::new(TypeVar::Unbound { id: 2 })),
                    alias: None,
                }),
            ),
            "fn(?) -> ?",
        );
        assert_string!(
            Type::function(
                vec![Rc::new(Type::Var {
                    tipo: Rc::new(RefCell::new(TypeVar::Generic { id: 78 })),
                    alias: None,
                })],
                Rc::new(Type::Var {
                    tipo: Rc::new(RefCell::new(TypeVar::Generic { id: 2 })),
                    alias: None,
                }),
            ),
            "fn(a) -> b",
        );
        assert_string!(
            Type::Fn {
                args: vec![Rc::new(Type::App {
                    public: true,
                    contains_opaque: false,
                    module: "".to_string(),
                    name: "PRNG".to_string(),
                    args: vec![],
                    alias: None,
                })],
                ret: Rc::new(Type::App {
                    public: true,
                    contains_opaque: false,
                    module: "".to_string(),
                    name: "Option".to_string(),
                    args: vec![Rc::new(Type::Tuple {
                        elems: vec![
                            Rc::new(Type::App {
                                public: true,
                                contains_opaque: false,
                                module: "".to_string(),
                                name: "PRNG".to_string(),
                                args: vec![],
                                alias: None,
                            }),
                            Rc::new(Type::App {
                                public: true,
                                contains_opaque: false,
                                module: "".to_string(),
                                name: "Bool".to_string(),
                                args: vec![],
                                alias: None,
                            }),
                        ],
                        alias: None,
                    })],
                    alias: None,
                }),
                alias: Some(Rc::new(TypeAliasAnnotation {
                    alias: "Fuzzer".to_string(),
                    parameters: vec!["a".to_string(),],
                    annotation: Annotation::Fn {
                        location: Span::empty(),
                        arguments: vec![Annotation::Constructor {
                            location: Span::empty(),
                            module: None,
                            name: "PRNG".to_string(),
                            arguments: vec![],
                        },],
                        ret: Box::new(Annotation::Constructor {
                            location: Span::empty(),
                            module: None,
                            name: "Option".to_string(),
                            arguments: vec![Annotation::Tuple {
                                location: Span::empty(),
                                elems: vec![
                                    Annotation::Constructor {
                                        location: Span::empty(),
                                        module: None,
                                        name: "PRNG".to_string(),
                                        arguments: vec![],
                                    },
                                    Annotation::Var {
                                        location: Span::empty(),
                                        name: "a".to_string(),
                                    },
                                ],
                            }],
                        }),
                    },
                })),
            },
            "Fuzzer<Bool>",
        );
        assert_string!(
            Type::Fn {
                args: vec![Rc::new(Type::App {
                    public: true,
                    contains_opaque: false,
                    module: "".to_string(),
                    name: "PRNG".to_string(),
                    args: vec![],
                    alias: None,
                })],
                ret: Rc::new(Type::App {
                    public: true,
                    contains_opaque: false,
                    module: "".to_string(),
                    name: "Option".to_string(),
                    args: vec![Rc::new(Type::Tuple {
                        elems: vec![
                            Rc::new(Type::App {
                                public: true,
                                contains_opaque: false,
                                module: "".to_string(),
                                name: "PRNG".to_string(),
                                args: vec![],
                                alias: None,
                            }),
                            Rc::new(Type::Var {
                                tipo: Rc::new(RefCell::new(TypeVar::Generic { id: 0 })),
                                alias: None,
                            }),
                        ],
                        alias: None,
                    })],
                    alias: None,
                }),
                alias: Some(Rc::new(TypeAliasAnnotation {
                    alias: "Fuzzer".to_string(),
                    parameters: vec!["a".to_string(),],
                    annotation: Annotation::Fn {
                        location: Span::empty(),
                        arguments: vec![Annotation::Constructor {
                            location: Span::empty(),
                            module: None,
                            name: "PRNG".to_string(),
                            arguments: vec![],
                        },],
                        ret: Box::new(Annotation::Constructor {
                            location: Span::empty(),
                            module: None,
                            name: "Option".to_string(),
                            arguments: vec![Annotation::Tuple {
                                location: Span::empty(),
                                elems: vec![
                                    Annotation::Constructor {
                                        location: Span::empty(),
                                        module: None,
                                        name: "PRNG".to_string(),
                                        arguments: vec![],
                                    },
                                    Annotation::Var {
                                        location: Span::empty(),
                                        name: "a".to_string(),
                                    },
                                ],
                            }],
                        }),
                    },
                })),
            },
            "Fuzzer<a>",
        );
        assert_string!(
            Rc::new(Type::Fn {
                args: vec![Rc::new(Type::App {
                    public: true,
                    contains_opaque: false,
                    module: "".to_string(),
                    name: "Bool".to_string(),
                    args: vec![],
                    alias: None,
                })],
                ret: Rc::new(Type::App {
                    public: true,
                    contains_opaque: false,
                    module: "".to_string(),
                    name: "Bool".to_string(),
                    args: vec![],
                    alias: None,
                }),
                alias: Some(Rc::new(TypeAliasAnnotation {
                    alias: "Identity".to_string(),
                    parameters: vec!["t".to_string()],
                    annotation: Annotation::Var {
                        location: Span::empty(),
                        name: "t".to_string(),
                    },
                })),
            }),
            "Identity<fn(Bool) -> Bool>",
        );
    }

    #[test]
    fn function_test() {
        assert_eq!(
            pretty_print(Type::function(vec![], Type::int())),
            "fn() -> Int"
        );

        assert_eq!(
            pretty_print(Type::function(
                vec![Type::int(), Type::int(), Type::int()],
                Type::int()
            )),
            "fn(Int, Int, Int) -> Int"
        );
    }

    fn pretty_print(typ: Rc<Type>) -> String {
        Printer::new().pretty_print(&typ, 0)
    }
}
