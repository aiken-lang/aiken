use std::sync::Arc;

use uplc::builtins::DefaultFunction;

use crate::{
    tipo::{Type, ValueConstructor},
    IdGenerator,
};

use super::air::Air;

pub struct AirStack<'a> {
    pub id_gen: &'a mut IdGenerator,
    pub scope: Vec<u64>,
    pub air: Vec<Air>,
}

impl<'a> AirStack<'a> {
    pub fn new(id_gen: &'a mut IdGenerator) -> Self {
        AirStack {
            id_gen,
            scope: vec![id_gen.next()],
            air: vec![],
        }
    }

    pub fn with_scope(id_gen: &'a mut IdGenerator, scope: Vec<u64>) -> Self {
        AirStack {
            id_gen,
            scope,
            air: vec![],
        }
    }

    pub fn in_new_scope(&mut self) -> Self {
        let mut new_stack = AirStack::with_scope(&mut self.id_gen, self.scope.clone());

        new_stack.new_scope();

        new_stack
    }

    pub fn new_scope(&mut self) {
        self.scope.push(self.id_gen.next());
    }

    pub fn merge(&mut self, mut other: AirStack) {
        self.air.append(&mut other.air);
    }

    pub fn sequence(&mut self, stacks: Vec<AirStack>) {
        for stack in stacks {
            self.merge(stack)
        }
    }

    pub fn integer(&mut self, value: String) {
        self.air.push(Air::Int {
            scope: self.scope.clone(),
            value,
        });
    }

    pub fn string(&mut self, value: String) {
        self.air.push(Air::String {
            scope: self.scope.clone(),
            value,
        });
    }

    pub fn byte_array(&mut self, bytes: Vec<u8>) {
        self.air.push(Air::ByteArray {
            scope: self.scope.clone(),
            bytes,
        });
    }

    pub fn builtin(&mut self, func: DefaultFunction, tipo: Arc<Type>, args: Vec<AirStack>) {
        self.air.push(Air::Builtin {
            scope: self.scope.clone(),
            count: args.len(),
            func,
            tipo,
        });

        self.sequence(args);
    }

    pub fn var(
        &mut self,
        constructor: ValueConstructor,
        name: impl ToString,
        variant_name: impl ToString,
    ) {
        self.air.push(Air::Var {
            scope: self.scope.clone(),
            constructor,
            name: name.to_string(),
            variant_name: variant_name.to_string(),
        });
    }

    pub fn anonymous_function(&mut self, params: Vec<String>, body: AirStack) {
        self.air.push(Air::Fn {
            scope: self.scope.clone(),
            params,
        });

        self.merge(body);
    }

    pub fn list(&mut self, tipo: Arc<Type>, elements: Vec<AirStack>, tail: Option<AirStack>) {
        self.air.push(Air::List {
            scope: self.scope.clone(),
            count: elements.len(),
            tipo,
            tail: tail.is_some(),
        });

        self.sequence(elements);

        if let Some(tail) = tail {
            self.merge(tail);
        }
    }

    pub fn record(&mut self, tipo: Arc<Type>, tag: usize, fields: Vec<AirStack>) {
        self.air.push(Air::Record {
            scope: self.scope.clone(),
            tag,
            tipo,
            count: fields.len(),
        });

        self.sequence(fields);
    }

    pub fn call(&mut self, tipo: Arc<Type>, fun: AirStack, args: Vec<AirStack>) {
        self.air.push(Air::Call {
            scope: self.scope.clone(),
            count: args.len(),
            tipo,
        });

        self.merge(fun);

        self.sequence(args);
    }

    pub fn binop(
        &mut self,
        name: crate::ast::BinOp,
        tipo: Arc<Type>,
        left: AirStack,
        right: AirStack,
    ) {
        self.air.push(Air::BinOp {
            scope: self.scope.clone(),
            name,
            tipo,
        });

        self.merge(left);
        self.merge(right);
    }

    pub fn let_assignment(&mut self, name: impl ToString, value: AirStack) {
        self.air.push(Air::Let {
            scope: self.scope.clone(),
            name: name.to_string(),
        });

        self.merge(value);
    }

    pub fn wrap_data(&mut self, tipo: Arc<Type>) {
        self.air.push(Air::WrapData {
            scope: self.scope.clone(),
            tipo,
        })
    }

    pub fn un_wrap_data(&mut self, tipo: Arc<Type>) {
        self.air.push(Air::UnWrapData {
            scope: self.scope.clone(),
            tipo,
        })
    }
}
