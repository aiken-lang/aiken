use crate::IdGenerator;

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
            scope: vec![0],
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

    pub fn merge(mut self, other: AirStack) -> Self {
        self.air.extend(other.air.into_iter());

        self
    }

    pub fn int(self, value: String) -> Self {
        let mut air = self.air.clone();

        air.push(Air::Int {
            scope: self.scope.clone(),
            value,
        });

        AirStack {
            id_gen: self.id_gen,
            scope: self.scope,
            air,
        }
    }

    pub fn string(mut self, value: String) -> Self {
        self.air.push(Air::String {
            scope: self.scope.clone(),
            value,
        });

        self
    }

    pub fn byte_array(mut self, bytes: Vec<u8>) -> Self {
        self.air.push(Air::ByteArray {
            scope: self.scope.clone(),
            bytes,
        });

        self
    }

    // pub fn sequence(mut self, expressions: AirEnv) -> Self {}
}
