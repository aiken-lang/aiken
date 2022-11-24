use pretty::RcDoc;

use crate::{
    ast::{Constant, Program, Term, Type},
    flat::Binder,
    plutus_data_to_bytes,
};

impl<'a, T> Program<T>
where
    T: Binder<'a>,
{
    pub fn to_pretty(&self) -> String {
        let mut w = Vec::new();

        self.to_doc().render(80, &mut w).unwrap();

        String::from_utf8(w)
            .unwrap()
            .lines()
            // This is a hack to deal with blank newlines
            // that end up with a bunch of useless whitespace
            // because of the nesting
            .map(|l| {
                if l.chars().all(|c| c.is_whitespace()) {
                    "".to_string()
                } else {
                    l.to_string()
                }
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn to_doc(&self) -> RcDoc<()> {
        let version = format!("{}.{}.{}", self.version.0, self.version.1, self.version.2);

        RcDoc::text("(")
            .append(RcDoc::text("program"))
            .append(RcDoc::line())
            .append(RcDoc::text(version))
            .append(RcDoc::line())
            .append(self.term.to_doc())
            .nest(2)
            .append(RcDoc::line_())
            .append(RcDoc::text(")"))
    }
}

impl<'a, T> Term<T>
where
    T: Binder<'a>,
{
    pub fn to_pretty(&self) -> String {
        let mut w = Vec::new();

        self.to_doc().render(80, &mut w).unwrap();

        String::from_utf8(w)
            .unwrap()
            .lines()
            // This is a hack to deal with blank newlines
            // that end up with a bunch of useless whitespace
            // because of the nesting
            .map(|l| {
                if l.chars().all(|c| c.is_whitespace()) {
                    "".to_string()
                } else {
                    l.to_string()
                }
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Term::Var(name) => RcDoc::text(name.text()),
            Term::Delay(term) => RcDoc::text("(")
                .append(
                    RcDoc::text("delay")
                        .append(RcDoc::line())
                        .append(term.to_doc())
                        .nest(2),
                )
                .append(RcDoc::line_())
                .append(RcDoc::text(")")),
            Term::Lambda {
                parameter_name,
                body,
            } => RcDoc::text("(")
                .append(
                    RcDoc::text("lam")
                        .append(RcDoc::line())
                        .append(RcDoc::text(parameter_name.text()))
                        .append(RcDoc::line())
                        .append(body.to_doc())
                        .nest(2),
                )
                .append(RcDoc::line_())
                .append(RcDoc::text(")")),
            Term::Apply { function, argument } => RcDoc::text("[")
                .append(
                    RcDoc::line()
                        .append(
                            function
                                .to_doc()
                                .append(RcDoc::line())
                                .append(argument.to_doc())
                                .group(),
                        )
                        .nest(2),
                )
                .append(RcDoc::line())
                .append(RcDoc::text("]")),
            Term::Constant(constant) => RcDoc::text("(")
                .append(
                    RcDoc::text("con")
                        .append(RcDoc::line())
                        .append(constant.to_doc())
                        .nest(2),
                )
                .append(RcDoc::line_())
                .append(RcDoc::text(")")),
            Term::Force(term) => RcDoc::text("(")
                .append(
                    RcDoc::text("force")
                        .append(RcDoc::line())
                        .append(term.to_doc())
                        .nest(2),
                )
                .append(RcDoc::line_())
                .append(RcDoc::text(")")),
            Term::Error => RcDoc::text("(")
                .append(RcDoc::text("error").nest(2))
                .append(RcDoc::line())
                .append(RcDoc::line_())
                .append(RcDoc::text(")")),
            Term::Builtin(builtin) => RcDoc::text("(")
                .append(
                    RcDoc::text("builtin")
                        .append(RcDoc::line())
                        .append(RcDoc::text(builtin.to_string()))
                        .nest(2),
                )
                .append(RcDoc::line_())
                .append(RcDoc::text(")")),
        }
        .group()
    }
}

impl Constant {
    pub fn to_pretty(&self) -> String {
        let mut w = Vec::new();

        self.to_doc().render(80, &mut w).unwrap();

        String::from_utf8(w)
            .unwrap()
            .lines()
            // This is a hack to deal with blank newlines
            // that end up with a bunch of useless whitespace
            // because of the nesting
            .map(|l| {
                if l.chars().all(|c| c.is_whitespace()) {
                    "".to_string()
                } else {
                    l.to_string()
                }
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Constant::Integer(i) => RcDoc::text("integer")
                .append(RcDoc::line())
                .append(RcDoc::as_string(i)),
            Constant::ByteString(bs) => RcDoc::text("bytestring")
                .append(RcDoc::line())
                .append(RcDoc::text("#"))
                .append(RcDoc::text(hex::encode(bs))),
            Constant::String(s) => RcDoc::text("string")
                .append(RcDoc::line())
                .append(RcDoc::text("\""))
                .append(RcDoc::text(s))
                .append(RcDoc::text("\"")),
            Constant::Unit => RcDoc::text("unit")
                .append(RcDoc::line())
                .append(RcDoc::text("()")),
            Constant::Bool(b) => RcDoc::text("bool")
                .append(RcDoc::line())
                .append(RcDoc::text(if *b { "True" } else { "False" })),
            Constant::ProtoList(r#type, items) => RcDoc::text("list")
                .append(RcDoc::line_())
                .append(r#type.to_doc())
                .append(RcDoc::line())
                .append(RcDoc::text("["))
                .append(RcDoc::intersperse(
                    items.iter().map(|c| c.to_doc_list()),
                    RcDoc::text(","),
                ))
                .append(RcDoc::text("]")),
            Constant::ProtoPair(type_left, type_right, left, right) => RcDoc::text("pair")
                .append(RcDoc::line_())
                .append(RcDoc::text("<"))
                .append(type_left.to_doc())
                .append(RcDoc::text(", "))
                .append(type_right.to_doc())
                .append(RcDoc::text(">"))
                .append(RcDoc::line())
                .append(RcDoc::text("["))
                .append(left.to_doc_list())
                .append(RcDoc::text(","))
                .append(right.to_doc_list())
                .append(RcDoc::text("]")),
            d @ Constant::Data(_) => RcDoc::text("data ").append(d.to_doc_list()),
        }
    }

    fn to_doc_list(&self) -> RcDoc<()> {
        match self {
            Constant::Integer(i) => RcDoc::as_string(i),
            Constant::ByteString(bs) => RcDoc::text("#").append(RcDoc::text(hex::encode(bs))),
            Constant::String(s) => RcDoc::text("\"")
                .append(RcDoc::text(s))
                .append(RcDoc::text("\"")),
            Constant::Unit => RcDoc::text("()"),
            Constant::Bool(b) => RcDoc::text(if *b { "True" } else { "False" }),
            Constant::ProtoList(_, items) => RcDoc::text("[")
                .append(RcDoc::intersperse(
                    items.iter().map(|c| c.to_doc_list()),
                    RcDoc::text(","),
                ))
                .append(RcDoc::text("]")),
            Constant::ProtoPair(_, _, left, right) => RcDoc::text("(")
                .append((*left).to_doc_list())
                .append(RcDoc::text(", "))
                .append((*right).to_doc_list())
                .append(RcDoc::text(")")),

            Constant::Data(data) => RcDoc::text("#").append(RcDoc::text(hex::encode(
                plutus_data_to_bytes(data).unwrap(),
            ))),
        }
    }
}

impl Type {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Type::Bool => RcDoc::text("bool"),
            Type::Integer => RcDoc::text("integer"),
            Type::String => RcDoc::text("string"),
            Type::ByteString => RcDoc::text("bytestring"),
            Type::Unit => RcDoc::text("unit"),
            Type::List(r#type) => RcDoc::text("list")
                .append(RcDoc::text("<"))
                .append(r#type.to_doc())
                .append(RcDoc::text(">")),
            Type::Pair(l, r) => RcDoc::text("pair")
                .append(RcDoc::text("<"))
                .append(l.to_doc())
                .append(RcDoc::text(", "))
                .append(r.to_doc())
                .append(RcDoc::text(">")),
            Type::Data => RcDoc::text("data"),
        }
    }
}
