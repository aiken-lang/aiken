use crate::{pretty, ExBudget, Term};
use aiken_lang::ast::BinOp;
use std::{
    fmt::{self, Display},
    path::PathBuf,
};
use uplc::ast::{NamedDeBruijn, Program};

#[derive(Debug, Clone)]
pub struct Script {
    pub input_path: PathBuf,
    pub module: String,
    pub name: String,
    pub can_error: bool,
    pub program: Program<NamedDeBruijn>,
    pub evaluation_hint: Option<EvalHint>,
}

unsafe impl Send for Script {}

impl Script {
    pub fn new(
        input_path: PathBuf,
        module: String,
        name: String,
        can_error: bool,
        program: Program<NamedDeBruijn>,
        evaluation_hint: Option<EvalHint>,
    ) -> Script {
        Script {
            input_path,
            module,
            name,
            program,
            can_error,
            evaluation_hint,
        }
    }
}

#[derive(Debug, Clone)]
pub struct EvalHint {
    pub bin_op: BinOp,
    pub left: Program<NamedDeBruijn>,
    pub right: Program<NamedDeBruijn>,
    pub can_error: bool,
}

impl Display for EvalHint {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let unlimited_budget = ExBudget {
            mem: i64::MAX,
            cpu: i64::MAX,
        };

        let left = pretty::boxed(
            "left",
            &match self.left.clone().eval(unlimited_budget).result() {
                Ok(term) => format!("{term}"),
                Err(err) => format!("{err}"),
            },
        );
        let right = pretty::boxed(
            "right",
            &match self.right.clone().eval(unlimited_budget).result() {
                Ok(term) => format!("{term}"),
                Err(err) => format!("{err}"),
            },
        );
        let msg = if self.can_error {
            match self.bin_op {
                BinOp::And => Some(format!(
                    "{left}\n\nand\n\n{right}\n\nare both true but shouldn't."
                )),
                BinOp::Or => Some(format!(
                    "neither\n\n{left}\n\nnor\n\n{right}\n\nshould be true."
                )),
                BinOp::Eq => Some(format!("{left}\n\nshould not be equal to\n\n{right}")),
                BinOp::NotEq => Some(format!("{left}\n\nshould be equal to\n\n{right}")),
                BinOp::LtInt => Some(format!(
                    "{left}\n\nshould be greater than or equal to\n\n{right}"
                )),
                BinOp::LtEqInt => Some(format!("{left}\n\nshould be greater than\n\n{right}")),
                BinOp::GtEqInt => Some(format!(
                    "{left}\n\nshould be lower than or equal\n\n{right}"
                )),
                BinOp::GtInt => Some(format!("{left}\n\nshould be lower than\n\n{right}")),
                _ => None,
            }
        } else {
            match self.bin_op {
                BinOp::And => Some(format!("{left}\n\nand\n\n{right}\n\nshould both be true.")),
                BinOp::Or => Some(format!("{left}\n\nor\n\n{right}\n\nshould be true.")),
                BinOp::Eq => Some(format!("{left}\n\nshould be equal to\n\n{right}")),
                BinOp::NotEq => Some(format!("{left}\n\nshould not be equal to\n\n{right}")),
                BinOp::LtInt => Some(format!("{left}\n\nshould be lower than\n\n{right}")),
                BinOp::LtEqInt => Some(format!(
                    "{left}\n\nshould be lower than or equal to\n\n{right}"
                )),
                BinOp::GtEqInt => Some(format!("{left}\n\nshould be greater than\n\n{right}")),
                BinOp::GtInt => Some(format!(
                    "{left}\n\nshould be greater than or equal to\n\n{right}"
                )),
                _ => None,
            }
        }
        .ok_or(fmt::Error)?;

        f.write_str(&msg)
    }
}

#[derive(Debug)]
pub struct EvalInfo {
    pub success: bool,
    pub script: Script,
    pub spent_budget: ExBudget,
    pub output: Option<Term<NamedDeBruijn>>,
    pub logs: Vec<String>,
}

unsafe impl Send for EvalInfo {}
