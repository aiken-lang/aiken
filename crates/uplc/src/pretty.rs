use pretty::RcDoc;

use crate::ast::{Constant, Name, Program, Term};

impl Program<Name> {
    pub fn to_pretty(&self) -> String {
        let mut w = Vec::new();

        self.to_doc().render(80, &mut w).unwrap();

        String::from_utf8(w).unwrap()
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

impl Term<Name> {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Term::Var(name) => RcDoc::text(format!("{}_{}", name.text, name.unique)),
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
                        .append(RcDoc::text(format!(
                            "{}_{}",
                            parameter_name.text, parameter_name.unique
                        )))
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
            Constant::Char(c) => unimplemented!("char: {}", c),
            Constant::Unit => RcDoc::text("unit")
                .append(RcDoc::line())
                .append(RcDoc::text("()")),
            Constant::Bool(b) => RcDoc::text("bool")
                .append(RcDoc::line())
                .append(RcDoc::text(if *b { "true" } else { "false" })),
        }
    }
}
