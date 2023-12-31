use crate::{
    ast::{Constant, Program, Term, Type},
    flat::Binder,
    machine::runtime::{convert_tag_to_constr, Compressable},
    machine::value::from_pallas_bigint,
};
use pallas_primitives::babbage::{Constr, PlutusData};
use pretty::RcDoc;
use std::ascii::escape_default;

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
            Term::Constr { tag, fields } => RcDoc::text("(")
                .append(
                    RcDoc::text("constr")
                        .append(RcDoc::line())
                        .append(RcDoc::as_string(tag))
                        .nest(2),
                )
                .append(RcDoc::line_())
                .append(RcDoc::intersperse(
                    fields.iter().map(|f| f.to_doc()),
                    RcDoc::line_(),
                ))
                .append(RcDoc::text(")")),
            Term::Case { constr, branches } => RcDoc::text("(")
                .append(
                    RcDoc::text("case")
                        .append(RcDoc::line())
                        .append(constr.to_doc())
                        .nest(2),
                )
                .append(RcDoc::line_())
                .append(RcDoc::intersperse(
                    branches.iter().map(|f| f.to_doc()),
                    RcDoc::line_(),
                ))
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
                .append(RcDoc::text(
                    String::from_utf8(
                        s.as_bytes()
                            .iter()
                            .flat_map(|c| escape_default(*c).collect::<Vec<u8>>())
                            .collect(),
                    )
                    .unwrap(),
                ))
                .append(RcDoc::text("\"")),
            Constant::Unit => RcDoc::text("unit")
                .append(RcDoc::line())
                .append(RcDoc::text("()")),
            Constant::Bool(b) => RcDoc::text("bool")
                .append(RcDoc::line())
                .append(RcDoc::text(if *b { "True" } else { "False" })),
            Constant::ProtoList(r#type, items) => RcDoc::text("(")
                .append("list")
                .append(RcDoc::space())
                .append(r#type.to_doc())
                .append(")")
                .append(RcDoc::line())
                .append(RcDoc::text("["))
                .append(RcDoc::intersperse(
                    items.iter().map(|c| c.to_doc_list()),
                    RcDoc::text(", "),
                ))
                .append(RcDoc::text("]")),
            Constant::ProtoPair(type_left, type_right, left, right) => RcDoc::text("(")
                .append("pair")
                .append(RcDoc::space())
                .append(type_left.to_doc())
                .append(RcDoc::space())
                .append(type_right.to_doc())
                .append(RcDoc::text(")"))
                .append(RcDoc::line())
                .append(RcDoc::text("("))
                .append(left.to_doc_list())
                .append(RcDoc::text(", "))
                .append(right.to_doc_list())
                .append(RcDoc::text(")")),
            Constant::Data(d) => RcDoc::text("data ")
                .append(RcDoc::text("("))
                .append(Self::to_doc_list_plutus_data(d))
                .append(RcDoc::text(")")),
            Constant::Bls12_381G1Element(p1) => RcDoc::text("bls12_381_G1_element ")
                .append(RcDoc::line())
                .append(RcDoc::text("0x"))
                .append(RcDoc::text(hex::encode(p1.compress()))),
            Constant::Bls12_381G2Element(p2) => RcDoc::text("bls12_381_G2_element ")
                .append(RcDoc::line())
                .append(RcDoc::text("0x"))
                .append(RcDoc::text(hex::encode(p2.compress()))),
            Constant::Bls12_381MlResult(_) => panic!("cannot represent Bls12_381MlResult as text"),
        }
    }

    fn to_doc_list(&self) -> RcDoc<()> {
        match self {
            Constant::Integer(i) => RcDoc::as_string(i),
            Constant::ByteString(bs) => RcDoc::text("#").append(RcDoc::text(hex::encode(bs))),
            Constant::String(s) => RcDoc::text("\"")
                .append(RcDoc::text(
                    String::from_utf8(
                        s.as_bytes()
                            .iter()
                            .flat_map(|c| escape_default(*c).collect::<Vec<u8>>())
                            .collect(),
                    )
                    .unwrap(),
                ))
                .append(RcDoc::text("\"")),
            Constant::Unit => RcDoc::text("()"),
            Constant::Bool(b) => RcDoc::text(if *b { "True" } else { "False" }),
            Constant::ProtoList(_, items) => RcDoc::text("[")
                .append(RcDoc::intersperse(
                    items.iter().map(|c| c.to_doc_list()),
                    RcDoc::text(", "),
                ))
                .append(RcDoc::text("]")),
            Constant::ProtoPair(_, _, left, right) => RcDoc::text("(")
                .append((*left).to_doc_list())
                .append(RcDoc::text(", "))
                .append((*right).to_doc_list())
                .append(RcDoc::text(")")),

            Constant::Data(data) => Self::to_doc_list_plutus_data(data),
            Constant::Bls12_381G1Element(p1) => {
                RcDoc::text("0x").append(RcDoc::text(hex::encode(p1.compress())))
            }
            Constant::Bls12_381G2Element(p2) => {
                RcDoc::text("0x").append(RcDoc::text(hex::encode(p2.compress())))
            }
            Constant::Bls12_381MlResult(_) => panic!("cannot represent Bls12_381MlResult as text"),
        }
    }

    // This feels a little awkward here; not sure if it should be upstreamed to pallas
    fn to_doc_list_plutus_data(data: &PlutusData) -> RcDoc<()> {
        match data {
            PlutusData::Constr(Constr {
                tag,
                any_constructor,
                fields,
            }) => RcDoc::text("Constr")
                .append(RcDoc::space())
                .append(RcDoc::as_string(
                    convert_tag_to_constr(*tag).unwrap_or_else(|| any_constructor.unwrap()),
                ))
                .append(RcDoc::space())
                .append(RcDoc::text("["))
                .append(RcDoc::intersperse(
                    fields.iter().map(Self::to_doc_list_plutus_data),
                    RcDoc::text(", "),
                ))
                .append(RcDoc::text("]")),
            PlutusData::Map(kvp) => RcDoc::text("Map")
                .append(RcDoc::space())
                .append(RcDoc::text("["))
                .append(RcDoc::intersperse(
                    kvp.iter().map(|(key, value)| {
                        RcDoc::text("(")
                            .append(Self::to_doc_list_plutus_data(key))
                            .append(RcDoc::text(", "))
                            .append(Self::to_doc_list_plutus_data(value))
                            .append(RcDoc::text(")"))
                    }),
                    RcDoc::text(", "),
                ))
                .append(RcDoc::text("]")),
            PlutusData::BigInt(bi) => RcDoc::text("I")
                .append(RcDoc::space())
                .append(RcDoc::text(from_pallas_bigint(bi).to_string())),
            PlutusData::BoundedBytes(bs) => RcDoc::text("B")
                .append(RcDoc::space())
                .append(RcDoc::text("#"))
                .append(RcDoc::text(hex::encode(bs.to_vec()))),
            PlutusData::Array(a) => RcDoc::text("List")
                .append(RcDoc::space())
                .append(RcDoc::text("["))
                .append(RcDoc::intersperse(
                    a.iter().map(Self::to_doc_list_plutus_data),
                    RcDoc::text(", "),
                ))
                .append(RcDoc::text("]")),
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
            Type::List(r#type) => RcDoc::text("(list")
                .append(RcDoc::line())
                .append(r#type.to_doc())
                .append(RcDoc::line_())
                .append(")"),
            Type::Pair(l, r) => RcDoc::text("(pair")
                .append(RcDoc::line())
                .append(l.to_doc())
                .append(RcDoc::line())
                .append(r.to_doc())
                .append(")"),
            Type::Data => RcDoc::text("data"),
            Type::Bls12_381G1Element => RcDoc::text("bls12_381_G1_element"),
            Type::Bls12_381G2Element => RcDoc::text("bls12_381_G1_element"),
            Type::Bls12_381MlResult => RcDoc::text("bls12_381_mlresult"),
        }
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    #[test]
    fn format_pair_bool_pair_integer_bytestring() {
        let uplc = "(program 0.0.0 (con (pair bool (pair integer bytestring)) (True, (14, #42))))";

        assert_eq!(
            crate::parser::program(uplc).unwrap().to_pretty(),
            indoc! {
                r#"
                (program
                  0.0.0
                  (con (pair bool (pair integer bytestring)) (True, (14, #42)))
                )"#
            }
        )
    }
}
