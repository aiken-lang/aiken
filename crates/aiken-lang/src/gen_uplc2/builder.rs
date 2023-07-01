use crate::builtins::bool;
use std::sync::Arc;

use crate::{
    ast::{BinOp, ClauseGuard, Constant, UnOp},
    tipo::Type,
};

use super::tree::AirTree;

pub fn convert_opaque_type() -> Arc<Type> {
    todo!()
}

pub fn constants_ir(literal: &Constant) -> AirTree {
    match literal {
        Constant::Int { value, .. } => AirTree::int(value),
        Constant::String { value, .. } => AirTree::string(value),
        Constant::ByteArray { bytes, .. } => AirTree::byte_array(bytes.clone()),
    }
}

pub fn handle_clause_guard(clause_guard: &ClauseGuard<Arc<Type>>) -> AirTree {
    match clause_guard {
        ClauseGuard::Not { value, .. } => {
            let val = handle_clause_guard(value);

            AirTree::unop(UnOp::Not, val)
        }
        ClauseGuard::Equals { left, right, .. } => {
            let left = handle_clause_guard(left);
            let right = handle_clause_guard(right);

            AirTree::binop(BinOp::Eq, bool(), left, right)
        }
        ClauseGuard::NotEquals { left, right, .. } => {
            let left = handle_clause_guard(left);
            let right = handle_clause_guard(right);

            AirTree::binop(BinOp::NotEq, bool(), left, right)
        }
        ClauseGuard::GtInt { left, right, .. } => {
            let left = handle_clause_guard(left);
            let right = handle_clause_guard(right);

            AirTree::binop(BinOp::GtInt, bool(), left, right)
        }
        ClauseGuard::GtEqInt { left, right, .. } => {
            let left = handle_clause_guard(left);
            let right = handle_clause_guard(right);

            AirTree::binop(BinOp::GtEqInt, bool(), left, right)
        }
        ClauseGuard::LtInt { left, right, .. } => {
            let left = handle_clause_guard(left);
            let right = handle_clause_guard(right);

            AirTree::binop(BinOp::LtInt, bool(), left, right)
        }
        ClauseGuard::LtEqInt { left, right, .. } => {
            let left = handle_clause_guard(left);
            let right = handle_clause_guard(right);

            AirTree::binop(BinOp::LtEqInt, bool(), left, right)
        }
        ClauseGuard::Or { left, right, .. } => {
            let left = handle_clause_guard(left);
            let right = handle_clause_guard(right);

            AirTree::binop(BinOp::Or, bool(), left, right)
        }
        ClauseGuard::And { left, right, .. } => {
            let left = handle_clause_guard(left);
            let right = handle_clause_guard(right);

            AirTree::binop(BinOp::And, bool(), left, right)
        }
        ClauseGuard::Var { tipo, name, .. } => AirTree::local_var(name, tipo.clone()),
        ClauseGuard::Constant(constant) => constants_ir(constant),
    }
}
