use super::{Error, Value};
use crate::builtins::DefaultFunction;
use num_traits::Signed;
use pallas_primitives::conway::Language;
use std::collections::HashMap;

macro_rules! hashmap {
    // map-like
    ($($k:expr => $v:expr),* $(,)?) => {{
        core::convert::From::from([$(($k, $v),)*])
    }};
    // set-like
    ($($v:expr),* $(,)?) => {{
        core::convert::From::from([$($v,)*])
    }};
}

/// Can be negative
#[derive(Debug, Clone, PartialEq, Eq, Copy, serde::Serialize)]
pub struct ExBudget {
    pub mem: i64,
    pub cpu: i64,
}

impl ExBudget {
    pub fn occurrences(&mut self, n: i64) {
        self.mem *= n;
        self.cpu *= n;
    }

    pub fn max() -> Self {
        ExBudget {
            mem: 14000000000000,
            cpu: 10000000000000,
        }
    }
}

impl Default for ExBudget {
    fn default() -> Self {
        ExBudget {
            mem: 14000000,
            cpu: 10000000000,
        }
    }
}

impl std::ops::Sub for ExBudget {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        ExBudget {
            mem: self.mem - rhs.mem,
            cpu: self.cpu - rhs.cpu,
        }
    }
}

#[derive(Default, Debug, PartialEq)]
pub struct CostModel {
    pub machine_costs: MachineCosts,
    pub builtin_costs: BuiltinCosts,
}

impl CostModel {
    pub fn v1() -> Self {
        Self {
            machine_costs: MachineCosts::v1(),
            builtin_costs: BuiltinCosts::v1(),
        }
    }

    pub fn v2() -> Self {
        Self {
            machine_costs: MachineCosts::v2(),
            builtin_costs: BuiltinCosts::v2(),
        }
    }

    pub fn v3() -> Self {
        Self {
            machine_costs: MachineCosts::v3(),
            builtin_costs: BuiltinCosts::v3(),
        }
    }
}

/// There's no entry for Error since we'll be exiting anyway; also, what would
/// happen if calling 'Error' caused the budget to be exceeded?
#[derive(Debug, PartialEq)]
pub struct MachineCosts {
    startup: ExBudget,
    var: ExBudget,
    constant: ExBudget,
    lambda: ExBudget,
    delay: ExBudget,
    force: ExBudget,
    apply: ExBudget,
    constr: ExBudget,
    case: ExBudget,
    /// Just the cost of evaluating a Builtin node, not the builtin itself.
    builtin: ExBudget,
}

impl MachineCosts {
    /// Get the cost of a step
    pub fn get(&self, step: StepKind) -> ExBudget {
        match step {
            StepKind::Constant => self.constant,
            StepKind::Var => self.var,
            StepKind::Lambda => self.lambda,
            StepKind::Apply => self.apply,
            StepKind::Delay => self.delay,
            StepKind::Force => self.force,
            StepKind::Builtin => self.builtin,
            StepKind::Constr => self.constr,
            StepKind::Case => self.case,
            StepKind::StartUp => self.startup,
        }
    }

    pub fn v1() -> Self {
        Self {
            startup: ExBudget { mem: 100, cpu: 100 },
            var: ExBudget {
                mem: 100,
                cpu: 16000,
            },
            constant: ExBudget {
                mem: 100,
                cpu: 16000,
            },
            lambda: ExBudget {
                mem: 100,
                cpu: 16000,
            },
            delay: ExBudget {
                mem: 100,
                cpu: 16000,
            },
            force: ExBudget {
                mem: 100,
                cpu: 16000,
            },
            apply: ExBudget {
                mem: 100,
                cpu: 16000,
            },
            builtin: ExBudget {
                mem: 100,
                cpu: 16000,
            },
            // Placeholder values
            constr: ExBudget {
                mem: 30000000000,
                cpu: 30000000000,
            },
            case: ExBudget {
                mem: 30000000000,
                cpu: 30000000000,
            },
        }
    }

    pub fn v2() -> Self {
        Self {
            startup: ExBudget { mem: 100, cpu: 100 },
            var: ExBudget {
                mem: 100,
                cpu: 16000,
            },
            constant: ExBudget {
                mem: 100,
                cpu: 16000,
            },
            lambda: ExBudget {
                mem: 100,
                cpu: 16000,
            },
            delay: ExBudget {
                mem: 100,
                cpu: 16000,
            },
            force: ExBudget {
                mem: 100,
                cpu: 16000,
            },
            apply: ExBudget {
                mem: 100,
                cpu: 16000,
            },
            builtin: ExBudget {
                mem: 100,
                cpu: 16000,
            },
            // Placeholder values
            constr: ExBudget {
                mem: 30000000000,
                cpu: 30000000000,
            },
            case: ExBudget {
                mem: 30000000000,
                cpu: 30000000000,
            },
        }
    }

    pub fn v3() -> Self {
        Self {
            startup: ExBudget { mem: 100, cpu: 100 },
            var: ExBudget {
                mem: 100,
                cpu: 16000,
            },
            constant: ExBudget {
                mem: 100,
                cpu: 16000,
            },
            lambda: ExBudget {
                mem: 100,
                cpu: 16000,
            },
            delay: ExBudget {
                mem: 100,
                cpu: 16000,
            },
            force: ExBudget {
                mem: 100,
                cpu: 16000,
            },
            apply: ExBudget {
                mem: 100,
                cpu: 16000,
            },
            builtin: ExBudget {
                mem: 100,
                cpu: 16000,
            },
            constr: ExBudget {
                mem: 100,
                cpu: 16000,
            },
            case: ExBudget {
                mem: 100,
                cpu: 16000,
            },
        }
    }
}

impl Default for MachineCosts {
    fn default() -> Self {
        MachineCosts::v3()
    }
}

#[derive(Debug, PartialEq)]
pub struct BuiltinCosts {
    pub add_integer: CostingFun<TwoArguments>,
    pub subtract_integer: CostingFun<TwoArguments>,
    pub multiply_integer: CostingFun<TwoArguments>,
    pub divide_integer: CostingFun<TwoArguments>,
    pub quotient_integer: CostingFun<TwoArguments>,
    pub remainder_integer: CostingFun<TwoArguments>,
    pub mod_integer: CostingFun<TwoArguments>,
    pub equals_integer: CostingFun<TwoArguments>,
    pub less_than_integer: CostingFun<TwoArguments>,
    pub less_than_equals_integer: CostingFun<TwoArguments>,
    // Bytestrings
    pub append_byte_string: CostingFun<TwoArguments>,
    pub cons_byte_string: CostingFun<TwoArguments>,
    pub slice_byte_string: CostingFun<ThreeArguments>,
    pub length_of_byte_string: CostingFun<OneArgument>,
    pub index_byte_string: CostingFun<TwoArguments>,
    pub equals_byte_string: CostingFun<TwoArguments>,
    pub less_than_byte_string: CostingFun<TwoArguments>,
    pub less_than_equals_byte_string: CostingFun<TwoArguments>,
    // Cryptography and hashes
    pub sha2_256: CostingFun<OneArgument>,
    pub sha3_256: CostingFun<OneArgument>,
    pub blake2b_224: CostingFun<OneArgument>,
    pub blake2b_256: CostingFun<OneArgument>,
    pub keccak_256: CostingFun<OneArgument>,
    pub verify_ed25519_signature: CostingFun<ThreeArguments>,
    pub verify_ecdsa_secp256k1_signature: CostingFun<ThreeArguments>,
    pub verify_schnorr_secp256k1_signature: CostingFun<ThreeArguments>,
    // Strings
    pub append_string: CostingFun<TwoArguments>,
    pub equals_string: CostingFun<TwoArguments>,
    pub encode_utf8: CostingFun<OneArgument>,
    pub decode_utf8: CostingFun<OneArgument>,
    // Bool
    pub if_then_else: CostingFun<ThreeArguments>,
    // Unit
    pub choose_unit: CostingFun<TwoArguments>,
    // Tracing
    pub trace: CostingFun<TwoArguments>,
    // Pairs
    pub fst_pair: CostingFun<OneArgument>,
    pub snd_pair: CostingFun<OneArgument>,
    // Lists
    pub choose_list: CostingFun<ThreeArguments>,
    pub mk_cons: CostingFun<TwoArguments>,
    pub head_list: CostingFun<OneArgument>,
    pub tail_list: CostingFun<OneArgument>,
    pub null_list: CostingFun<OneArgument>,
    // Data
    pub choose_data: CostingFun<SixArguments>,
    pub constr_data: CostingFun<TwoArguments>,
    pub map_data: CostingFun<OneArgument>,
    pub list_data: CostingFun<OneArgument>,
    pub i_data: CostingFun<OneArgument>,
    pub b_data: CostingFun<OneArgument>,
    pub un_constr_data: CostingFun<OneArgument>,
    pub un_map_data: CostingFun<OneArgument>,
    pub un_list_data: CostingFun<OneArgument>,
    pub un_i_data: CostingFun<OneArgument>,
    pub un_b_data: CostingFun<OneArgument>,
    pub equals_data: CostingFun<TwoArguments>,
    // Misc constructors
    pub mk_pair_data: CostingFun<TwoArguments>,
    pub mk_nil_data: CostingFun<OneArgument>,
    pub mk_nil_pair_data: CostingFun<OneArgument>,
    pub serialise_data: CostingFun<OneArgument>,
    // BLST
    bls12_381_g1_add: CostingFun<TwoArguments>,
    bls12_381_g1_neg: CostingFun<OneArgument>,
    bls12_381_g1_scalar_mul: CostingFun<TwoArguments>,
    bls12_381_g1_equal: CostingFun<TwoArguments>,
    bls12_381_g1_compress: CostingFun<OneArgument>,
    bls12_381_g1_uncompress: CostingFun<OneArgument>,
    bls12_381_g1_hash_to_group: CostingFun<TwoArguments>,
    bls12_381_g2_add: CostingFun<TwoArguments>,
    bls12_381_g2_neg: CostingFun<OneArgument>,
    bls12_381_g2_scalar_mul: CostingFun<TwoArguments>,
    bls12_381_g2_equal: CostingFun<TwoArguments>,
    bls12_381_g2_compress: CostingFun<OneArgument>,
    bls12_381_g2_uncompress: CostingFun<OneArgument>,
    bls12_381_g2_hash_to_group: CostingFun<TwoArguments>,
    bls12_381_miller_loop: CostingFun<TwoArguments>,
    bls12_381_mul_ml_result: CostingFun<TwoArguments>,
    bls12_381_final_verify: CostingFun<TwoArguments>,
    // bitwise
    integer_to_byte_string: CostingFun<ThreeArguments>,
    byte_string_to_integer: CostingFun<TwoArguments>,
    and_byte_string: CostingFun<ThreeArguments>,
    or_byte_string: CostingFun<ThreeArguments>,
    xor_byte_string: CostingFun<ThreeArguments>,
    complement_byte_string: CostingFun<OneArgument>,
    read_bit: CostingFun<TwoArguments>,
    write_bits: CostingFun<ThreeArguments>,
    replicate_byte: CostingFun<TwoArguments>,
    shift_byte_string: CostingFun<TwoArguments>,
    rotate_byte_string: CostingFun<TwoArguments>,
    count_set_bits: CostingFun<OneArgument>,
    find_first_set_bit: CostingFun<OneArgument>,
    ripemd_160: CostingFun<OneArgument>,
    exp_mod_int: CostingFun<ThreeArguments>,
}

impl BuiltinCosts {
    pub fn v1() -> Self {
        Self {
            add_integer: CostingFun {
                mem: TwoArguments::MaxSize(MaxSize {
                    intercept: 1,
                    slope: 1,
                }),
                cpu: TwoArguments::MaxSize(MaxSize {
                    intercept: 100788,
                    slope: 420,
                }),
            },
            subtract_integer: CostingFun {
                mem: TwoArguments::MaxSize(MaxSize {
                    intercept: 1,
                    slope: 1,
                }),
                cpu: TwoArguments::MaxSize(MaxSize {
                    intercept: 100788,
                    slope: 420,
                }),
            },
            multiply_integer: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 0,
                    slope: 1,
                }),
                cpu: TwoArguments::MultipliedSizes(MultipliedSizes {
                    intercept: 90434,
                    slope: 519,
                }),
            },
            divide_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: 0,
                    slope: 1,
                    minimum: 1,
                }),
                cpu: TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                    constant: 85848,
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: 228465,
                        slope: 122,
                    })),
                }),
            },
            quotient_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: 0,
                    slope: 1,
                    minimum: 1,
                }),
                cpu: TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                    constant: 85848,
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: 228465,
                        slope: 122,
                    })),
                }),
            },
            remainder_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: 0,
                    slope: 1,
                    minimum: 1,
                }),
                cpu: TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                    constant: 85848,
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: 228465,
                        slope: 122,
                    })),
                }),
            },
            mod_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: 0,
                    slope: 1,
                    minimum: 1,
                }),
                cpu: TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                    constant: 85848,
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: 228465,
                        slope: 122,
                    })),
                }),
            },
            equals_integer: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 51775,
                    slope: 558,
                }),
            },
            less_than_integer: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 44749,
                    slope: 541,
                }),
            },
            less_than_equals_integer: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 43285,
                    slope: 552,
                }),
            },
            append_byte_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 0,
                    slope: 1,
                }),
                cpu: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 1000,
                    slope: 173,
                }),
            },
            cons_byte_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 0,
                    slope: 1,
                }),
                cpu: TwoArguments::LinearInY(LinearSize {
                    intercept: 72010,
                    slope: 178,
                }),
            },
            slice_byte_string: CostingFun {
                mem: ThreeArguments::LinearInZ(LinearSize {
                    intercept: 4,
                    slope: 0,
                }),
                cpu: ThreeArguments::LinearInZ(LinearSize {
                    intercept: 20467,
                    slope: 1,
                }),
            },
            length_of_byte_string: CostingFun {
                mem: OneArgument::ConstantCost(10),
                cpu: OneArgument::ConstantCost(22100),
            },
            index_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(4),
                cpu: TwoArguments::ConstantCost(13169),
            },
            equals_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::LinearOnDiagonal(ConstantOrLinear {
                    constant: 24548,
                    intercept: 29498,
                    slope: 38,
                }),
            },
            less_than_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 28999,
                    slope: 74,
                }),
            },
            less_than_equals_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 28999,
                    slope: 74,
                }),
            },
            sha2_256: CostingFun {
                mem: OneArgument::ConstantCost(4),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 270652,
                    slope: 22588,
                }),
            },
            sha3_256: CostingFun {
                mem: OneArgument::ConstantCost(4),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 1457325,
                    slope: 64566,
                }),
            },
            blake2b_224: CostingFun {
                mem: OneArgument::ConstantCost(30000000000),
                cpu: OneArgument::ConstantCost(30000000000),
            },
            blake2b_256: CostingFun {
                mem: OneArgument::ConstantCost(4),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 201305,
                    slope: 8356,
                }),
            },
            keccak_256: CostingFun {
                mem: OneArgument::ConstantCost(30000000000),
                cpu: OneArgument::ConstantCost(30000000000),
            },
            verify_ed25519_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(10),
                cpu: ThreeArguments::LinearInY(LinearSize {
                    intercept: 53384111,
                    slope: 14333,
                }),
            },
            verify_ecdsa_secp256k1_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(30000000000),
                cpu: ThreeArguments::ConstantCost(30000000000),
            },
            verify_schnorr_secp256k1_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(30000000000),
                cpu: ThreeArguments::LinearInY(LinearSize {
                    intercept: 30000000000,
                    slope: 30000000000,
                }),
            },
            append_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 4,
                    slope: 1,
                }),
                cpu: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 1000,
                    slope: 59957,
                }),
            },
            equals_string: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::LinearOnDiagonal(ConstantOrLinear {
                    constant: 39184,
                    intercept: 1000,
                    slope: 60594,
                }),
            },
            encode_utf8: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: 4,
                    slope: 2,
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 1000,
                    slope: 42921,
                }),
            },
            decode_utf8: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: 4,
                    slope: 2,
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 91189,
                    slope: 769,
                }),
            },
            if_then_else: CostingFun {
                mem: ThreeArguments::ConstantCost(1),
                cpu: ThreeArguments::ConstantCost(76049),
            },
            choose_unit: CostingFun {
                mem: TwoArguments::ConstantCost(4),
                cpu: TwoArguments::ConstantCost(61462),
            },
            trace: CostingFun {
                mem: TwoArguments::ConstantCost(32),
                cpu: TwoArguments::ConstantCost(59498),
            },
            fst_pair: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(141895),
            },
            snd_pair: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(141992),
            },
            choose_list: CostingFun {
                mem: ThreeArguments::ConstantCost(32),
                cpu: ThreeArguments::ConstantCost(132994),
            },
            mk_cons: CostingFun {
                mem: TwoArguments::ConstantCost(32),
                cpu: TwoArguments::ConstantCost(72362),
            },
            head_list: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(83150),
            },
            tail_list: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(81663),
            },
            null_list: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(74433),
            },
            choose_data: CostingFun {
                mem: SixArguments::ConstantCost(32),
                cpu: SixArguments::ConstantCost(94375),
            },
            constr_data: CostingFun {
                mem: TwoArguments::ConstantCost(32),
                cpu: TwoArguments::ConstantCost(22151),
            },
            map_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(68246),
            },
            list_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(33852),
            },
            i_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(15299),
            },
            b_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(11183),
            },
            un_constr_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(24588),
            },
            un_map_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(24623),
            },
            un_list_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(25933),
            },
            un_i_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(20744),
            },
            un_b_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(20142),
            },
            equals_data: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 898148,
                    slope: 27279,
                }),
            },
            mk_pair_data: CostingFun {
                mem: TwoArguments::ConstantCost(32),
                cpu: TwoArguments::ConstantCost(11546),
            },
            mk_nil_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(7243),
            },
            mk_nil_pair_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(7391),
            },
            serialise_data: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: 30000000000,
                    slope: 30000000000,
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 30000000000,
                    slope: 30000000000,
                }),
            },
            bls12_381_g1_add: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            bls12_381_g1_neg: CostingFun {
                cpu: OneArgument::ConstantCost(30000000000),
                mem: OneArgument::ConstantCost(30000000000),
            },
            bls12_381_g1_scalar_mul: CostingFun {
                mem: TwoArguments::ConstantCost(30000000000),
                cpu: TwoArguments::ConstantCost(30000000000),
            },
            bls12_381_g1_equal: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            bls12_381_g1_compress: CostingFun {
                cpu: OneArgument::ConstantCost(30000000000),
                mem: OneArgument::ConstantCost(30000000000),
            },
            bls12_381_g1_uncompress: CostingFun {
                cpu: OneArgument::ConstantCost(30000000000),
                mem: OneArgument::ConstantCost(30000000000),
            },
            bls12_381_g1_hash_to_group: CostingFun {
                mem: TwoArguments::ConstantCost(30000000000),
                cpu: TwoArguments::ConstantCost(30000000000),
            },
            bls12_381_g2_add: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            bls12_381_g2_neg: CostingFun {
                cpu: OneArgument::ConstantCost(30000000000),
                mem: OneArgument::ConstantCost(30000000000),
            },
            bls12_381_g2_scalar_mul: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            bls12_381_g2_equal: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            bls12_381_g2_compress: CostingFun {
                cpu: OneArgument::ConstantCost(30000000000),
                mem: OneArgument::ConstantCost(30000000000),
            },
            bls12_381_g2_uncompress: CostingFun {
                cpu: OneArgument::ConstantCost(30000000000),
                mem: OneArgument::ConstantCost(30000000000),
            },
            bls12_381_g2_hash_to_group: CostingFun {
                mem: TwoArguments::ConstantCost(30000000000),
                cpu: TwoArguments::ConstantCost(30000000000),
            },
            bls12_381_miller_loop: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            bls12_381_mul_ml_result: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            bls12_381_final_verify: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            integer_to_byte_string: CostingFun {
                cpu: ThreeArguments::ConstantCost(30000000000),
                mem: ThreeArguments::ConstantCost(30000000000),
            },
            byte_string_to_integer: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            and_byte_string: CostingFun {
                cpu: ThreeArguments::ConstantCost(30000000000),
                mem: ThreeArguments::ConstantCost(30000000000),
            },
            or_byte_string: CostingFun {
                cpu: ThreeArguments::ConstantCost(30000000000),
                mem: ThreeArguments::ConstantCost(30000000000),
            },
            xor_byte_string: CostingFun {
                cpu: ThreeArguments::ConstantCost(30000000000),
                mem: ThreeArguments::ConstantCost(30000000000),
            },
            complement_byte_string: CostingFun {
                cpu: OneArgument::ConstantCost(30000000000),
                mem: OneArgument::ConstantCost(30000000000),
            },
            read_bit: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            write_bits: CostingFun {
                cpu: ThreeArguments::ConstantCost(30000000000),
                mem: ThreeArguments::ConstantCost(30000000000),
            },
            replicate_byte: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            shift_byte_string: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            rotate_byte_string: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            count_set_bits: CostingFun {
                cpu: OneArgument::ConstantCost(30000000000),
                mem: OneArgument::ConstantCost(30000000000),
            },
            find_first_set_bit: CostingFun {
                cpu: OneArgument::ConstantCost(30000000000),
                mem: OneArgument::ConstantCost(30000000000),
            },
            ripemd_160: CostingFun {
                cpu: OneArgument::ConstantCost(30000000000),
                mem: OneArgument::ConstantCost(30000000000),
            },
            exp_mod_int: CostingFun {
                cpu: ThreeArguments::ConstantCost(30000000000),
                mem: ThreeArguments::ConstantCost(30000000000),
            },
        }
    }

    pub fn v2() -> Self {
        Self {
            add_integer: CostingFun {
                mem: TwoArguments::MaxSize(MaxSize {
                    intercept: 1,
                    slope: 1,
                }),
                cpu: TwoArguments::MaxSize(MaxSize {
                    intercept: 100788,
                    slope: 420,
                }),
            },
            subtract_integer: CostingFun {
                mem: TwoArguments::MaxSize(MaxSize {
                    intercept: 1,
                    slope: 1,
                }),
                cpu: TwoArguments::MaxSize(MaxSize {
                    intercept: 100788,
                    slope: 420,
                }),
            },
            multiply_integer: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 0,
                    slope: 1,
                }),
                cpu: TwoArguments::MultipliedSizes(MultipliedSizes {
                    intercept: 90434,
                    slope: 519,
                }),
            },
            divide_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: 0,
                    slope: 1,
                    minimum: 1,
                }),
                cpu: TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                    constant: 85848,
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: 228465,
                        slope: 122,
                    })),
                }),
            },
            quotient_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: 0,
                    slope: 1,
                    minimum: 1,
                }),
                cpu: TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                    constant: 85848,
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: 228465,
                        slope: 122,
                    })),
                }),
            },
            remainder_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: 0,
                    slope: 1,
                    minimum: 1,
                }),
                cpu: TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                    constant: 85848,
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: 228465,
                        slope: 122,
                    })),
                }),
            },
            mod_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: 0,
                    slope: 1,
                    minimum: 1,
                }),
                cpu: TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                    constant: 85848,
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: 228465,
                        slope: 122,
                    })),
                }),
            },
            equals_integer: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 51775,
                    slope: 558,
                }),
            },
            less_than_integer: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 44749,
                    slope: 541,
                }),
            },
            less_than_equals_integer: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 43285,
                    slope: 552,
                }),
            },
            append_byte_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 0,
                    slope: 1,
                }),
                cpu: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 1000,
                    slope: 173,
                }),
            },
            cons_byte_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 0,
                    slope: 1,
                }),
                cpu: TwoArguments::LinearInY(LinearSize {
                    intercept: 72010,
                    slope: 178,
                }),
            },
            slice_byte_string: CostingFun {
                mem: ThreeArguments::LinearInZ(LinearSize {
                    intercept: 4,
                    slope: 0,
                }),
                cpu: ThreeArguments::LinearInZ(LinearSize {
                    intercept: 20467,
                    slope: 1,
                }),
            },
            length_of_byte_string: CostingFun {
                mem: OneArgument::ConstantCost(10),
                cpu: OneArgument::ConstantCost(22100),
            },
            index_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(4),
                cpu: TwoArguments::ConstantCost(13169),
            },
            equals_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::LinearOnDiagonal(ConstantOrLinear {
                    constant: 24548,
                    intercept: 29498,
                    slope: 38,
                }),
            },
            less_than_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 28999,
                    slope: 74,
                }),
            },
            less_than_equals_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 28999,
                    slope: 74,
                }),
            },
            sha2_256: CostingFun {
                mem: OneArgument::ConstantCost(4),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 270652,
                    slope: 22588,
                }),
            },
            sha3_256: CostingFun {
                mem: OneArgument::ConstantCost(4),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 1457325,
                    slope: 64566,
                }),
            },
            blake2b_256: CostingFun {
                mem: OneArgument::ConstantCost(4),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 201305,
                    slope: 8356,
                }),
            },
            verify_ed25519_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(10),
                cpu: ThreeArguments::LinearInY(LinearSize {
                    intercept: 53384111,
                    slope: 14333,
                }),
            },
            verify_ecdsa_secp256k1_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(10),
                cpu: ThreeArguments::ConstantCost(43053543),
            },
            verify_schnorr_secp256k1_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(10),
                cpu: ThreeArguments::LinearInY(LinearSize {
                    intercept: 43574283,
                    slope: 26308,
                }),
            },
            append_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 4,
                    slope: 1,
                }),
                cpu: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 1000,
                    slope: 59957,
                }),
            },
            equals_string: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::LinearOnDiagonal(ConstantOrLinear {
                    constant: 39184,
                    intercept: 1000,
                    slope: 60594,
                }),
            },
            encode_utf8: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: 4,
                    slope: 2,
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 1000,
                    slope: 42921,
                }),
            },
            decode_utf8: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: 4,
                    slope: 2,
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 91189,
                    slope: 769,
                }),
            },
            if_then_else: CostingFun {
                mem: ThreeArguments::ConstantCost(1),
                cpu: ThreeArguments::ConstantCost(76049),
            },
            choose_unit: CostingFun {
                mem: TwoArguments::ConstantCost(4),
                cpu: TwoArguments::ConstantCost(61462),
            },
            trace: CostingFun {
                mem: TwoArguments::ConstantCost(32),
                cpu: TwoArguments::ConstantCost(59498),
            },
            fst_pair: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(141895),
            },
            snd_pair: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(141992),
            },
            choose_list: CostingFun {
                mem: ThreeArguments::ConstantCost(32),
                cpu: ThreeArguments::ConstantCost(132994),
            },
            mk_cons: CostingFun {
                mem: TwoArguments::ConstantCost(32),
                cpu: TwoArguments::ConstantCost(72362),
            },
            head_list: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(83150),
            },
            tail_list: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(81663),
            },
            null_list: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(74433),
            },
            choose_data: CostingFun {
                mem: SixArguments::ConstantCost(32),
                cpu: SixArguments::ConstantCost(94375),
            },
            constr_data: CostingFun {
                mem: TwoArguments::ConstantCost(32),
                cpu: TwoArguments::ConstantCost(22151),
            },
            map_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(68246),
            },
            list_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(33852),
            },
            i_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(15299),
            },
            b_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(11183),
            },
            un_constr_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(24588),
            },
            un_map_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(24623),
            },
            un_list_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(25933),
            },
            un_i_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(20744),
            },
            un_b_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(20142),
            },
            equals_data: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 898148,
                    slope: 27279,
                }),
            },
            mk_pair_data: CostingFun {
                mem: TwoArguments::ConstantCost(32),
                cpu: TwoArguments::ConstantCost(11546),
            },
            mk_nil_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(7243),
            },
            mk_nil_pair_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(7391),
            },
            serialise_data: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: 0,
                    slope: 2,
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 955506,
                    slope: 213312,
                }),
            },
            blake2b_224: CostingFun {
                cpu: OneArgument::ConstantCost(30000000000),
                mem: OneArgument::ConstantCost(30000000000),
            },
            keccak_256: CostingFun {
                cpu: OneArgument::ConstantCost(30000000000),
                mem: OneArgument::ConstantCost(30000000000),
            },
            bls12_381_g1_add: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            bls12_381_g1_neg: CostingFun {
                cpu: OneArgument::ConstantCost(30000000000),
                mem: OneArgument::ConstantCost(30000000000),
            },
            bls12_381_g1_scalar_mul: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            bls12_381_g1_equal: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            bls12_381_g1_compress: CostingFun {
                cpu: OneArgument::ConstantCost(30000000000),
                mem: OneArgument::ConstantCost(30000000000),
            },
            bls12_381_g1_uncompress: CostingFun {
                cpu: OneArgument::ConstantCost(30000000000),
                mem: OneArgument::ConstantCost(30000000000),
            },
            bls12_381_g1_hash_to_group: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            bls12_381_g2_add: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            bls12_381_g2_neg: CostingFun {
                cpu: OneArgument::ConstantCost(30000000000),
                mem: OneArgument::ConstantCost(30000000000),
            },
            bls12_381_g2_scalar_mul: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            bls12_381_g2_equal: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            bls12_381_g2_compress: CostingFun {
                cpu: OneArgument::ConstantCost(30000000000),
                mem: OneArgument::ConstantCost(30000000000),
            },
            bls12_381_g2_uncompress: CostingFun {
                cpu: OneArgument::ConstantCost(30000000000),
                mem: OneArgument::ConstantCost(30000000000),
            },
            bls12_381_g2_hash_to_group: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            bls12_381_miller_loop: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            bls12_381_mul_ml_result: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            bls12_381_final_verify: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            integer_to_byte_string: CostingFun {
                cpu: ThreeArguments::ConstantCost(30000000000),
                mem: ThreeArguments::ConstantCost(30000000000),
            },
            byte_string_to_integer: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            and_byte_string: CostingFun {
                cpu: ThreeArguments::ConstantCost(30000000000),
                mem: ThreeArguments::ConstantCost(30000000000),
            },
            or_byte_string: CostingFun {
                cpu: ThreeArguments::ConstantCost(30000000000),
                mem: ThreeArguments::ConstantCost(30000000000),
            },
            xor_byte_string: CostingFun {
                cpu: ThreeArguments::ConstantCost(30000000000),
                mem: ThreeArguments::ConstantCost(30000000000),
            },
            complement_byte_string: CostingFun {
                cpu: OneArgument::ConstantCost(30000000000),
                mem: OneArgument::ConstantCost(30000000000),
            },
            read_bit: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            write_bits: CostingFun {
                cpu: ThreeArguments::ConstantCost(30000000000),
                mem: ThreeArguments::ConstantCost(30000000000),
            },
            replicate_byte: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            shift_byte_string: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            rotate_byte_string: CostingFun {
                cpu: TwoArguments::ConstantCost(30000000000),
                mem: TwoArguments::ConstantCost(30000000000),
            },
            count_set_bits: CostingFun {
                cpu: OneArgument::ConstantCost(30000000000),
                mem: OneArgument::ConstantCost(30000000000),
            },
            find_first_set_bit: CostingFun {
                cpu: OneArgument::ConstantCost(30000000000),
                mem: OneArgument::ConstantCost(30000000000),
            },
            ripemd_160: CostingFun {
                cpu: OneArgument::ConstantCost(30000000000),
                mem: OneArgument::ConstantCost(30000000000),
            },
            exp_mod_int: CostingFun {
                cpu: ThreeArguments::ConstantCost(30000000000),
                mem: ThreeArguments::ConstantCost(30000000000),
            },
        }
    }

    pub fn v3() -> Self {
        Self {
            add_integer: CostingFun {
                mem: TwoArguments::MaxSize(MaxSize {
                    intercept: 1,
                    slope: 1,
                }),
                cpu: TwoArguments::MaxSize(MaxSize {
                    intercept: 100788,
                    slope: 420,
                }),
            },
            subtract_integer: CostingFun {
                mem: TwoArguments::MaxSize(MaxSize {
                    intercept: 1,
                    slope: 1,
                }),
                cpu: TwoArguments::MaxSize(MaxSize {
                    intercept: 100788,
                    slope: 420,
                }),
            },
            multiply_integer: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 0,
                    slope: 1,
                }),
                cpu: TwoArguments::MultipliedSizes(MultipliedSizes {
                    intercept: 90434,
                    slope: 519,
                }),
            },
            divide_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: 0,
                    slope: 1,
                    minimum: 1,
                }),
                cpu: TwoArguments::ConstAboveDiagonalIntoQuadraticXAndY(
                    85848,
                    TwoArgumentsQuadraticFunction {
                        minimum: 85848,
                        coeff_00: 123203,
                        coeff_01: 7305,
                        coeff_02: -900,
                        coeff_10: 1716,
                        coeff_11: 549,
                        coeff_20: 57,
                    },
                ),
            },
            quotient_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: 0,
                    slope: 1,
                    minimum: 1,
                }),
                cpu: TwoArguments::ConstAboveDiagonalIntoQuadraticXAndY(
                    85848,
                    TwoArgumentsQuadraticFunction {
                        minimum: 85848,
                        coeff_00: 123203,
                        coeff_01: 7305,
                        coeff_02: -900,
                        coeff_10: 1716,
                        coeff_11: 549,
                        coeff_20: 57,
                    },
                ),
            },
            remainder_integer: CostingFun {
                mem: TwoArguments::LinearInY(LinearSize {
                    intercept: 0,
                    slope: 1,
                }),
                cpu: TwoArguments::ConstAboveDiagonalIntoQuadraticXAndY(
                    85848,
                    TwoArgumentsQuadraticFunction {
                        minimum: 85848,
                        coeff_00: 123203,
                        coeff_01: 7305,
                        coeff_02: -900,
                        coeff_10: 1716,
                        coeff_11: 549,
                        coeff_20: 57,
                    },
                ),
            },
            mod_integer: CostingFun {
                mem: TwoArguments::LinearInY(LinearSize {
                    intercept: 0,
                    slope: 1,
                }),
                cpu: TwoArguments::ConstAboveDiagonalIntoQuadraticXAndY(
                    85848,
                    TwoArgumentsQuadraticFunction {
                        minimum: 85848,
                        coeff_00: 123203,
                        coeff_01: 7305,
                        coeff_02: -900,
                        coeff_10: 1716,
                        coeff_11: 549,
                        coeff_20: 57,
                    },
                ),
            },
            equals_integer: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 51775,
                    slope: 558,
                }),
            },
            less_than_integer: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 44749,
                    slope: 541,
                }),
            },
            less_than_equals_integer: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 43285,
                    slope: 552,
                }),
            },
            append_byte_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 0,
                    slope: 1,
                }),
                cpu: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 1000,
                    slope: 173,
                }),
            },
            cons_byte_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 0,
                    slope: 1,
                }),
                cpu: TwoArguments::LinearInY(LinearSize {
                    intercept: 72010,
                    slope: 178,
                }),
            },
            slice_byte_string: CostingFun {
                mem: ThreeArguments::LinearInZ(LinearSize {
                    intercept: 4,
                    slope: 0,
                }),
                cpu: ThreeArguments::LinearInZ(LinearSize {
                    intercept: 20467,
                    slope: 1,
                }),
            },
            length_of_byte_string: CostingFun {
                mem: OneArgument::ConstantCost(10),
                cpu: OneArgument::ConstantCost(22100),
            },
            index_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(4),
                cpu: TwoArguments::ConstantCost(13169),
            },
            equals_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::LinearOnDiagonal(ConstantOrLinear {
                    constant: 24548,
                    intercept: 29498,
                    slope: 38,
                }),
            },
            less_than_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 28999,
                    slope: 74,
                }),
            },
            less_than_equals_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 28999,
                    slope: 74,
                }),
            },
            sha2_256: CostingFun {
                mem: OneArgument::ConstantCost(4),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 270652,
                    slope: 22588,
                }),
            },
            sha3_256: CostingFun {
                mem: OneArgument::ConstantCost(4),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 1457325,
                    slope: 64566,
                }),
            },
            blake2b_256: CostingFun {
                mem: OneArgument::ConstantCost(4),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 201305,
                    slope: 8356,
                }),
            },
            verify_ed25519_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(10),
                cpu: ThreeArguments::LinearInY(LinearSize {
                    intercept: 53384111,
                    slope: 14333,
                }),
            },
            verify_ecdsa_secp256k1_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(10),
                cpu: ThreeArguments::ConstantCost(43053543),
            },
            verify_schnorr_secp256k1_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(10),
                cpu: ThreeArguments::LinearInY(LinearSize {
                    intercept: 43574283,
                    slope: 26308,
                }),
            },
            append_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 4,
                    slope: 1,
                }),
                cpu: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 1000,
                    slope: 59957,
                }),
            },
            equals_string: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::LinearOnDiagonal(ConstantOrLinear {
                    constant: 39184,
                    intercept: 1000,
                    slope: 60594,
                }),
            },
            encode_utf8: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: 4,
                    slope: 2,
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 1000,
                    slope: 42921,
                }),
            },
            decode_utf8: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: 4,
                    slope: 2,
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 91189,
                    slope: 769,
                }),
            },
            if_then_else: CostingFun {
                mem: ThreeArguments::ConstantCost(1),
                cpu: ThreeArguments::ConstantCost(76049),
            },
            choose_unit: CostingFun {
                mem: TwoArguments::ConstantCost(4),
                cpu: TwoArguments::ConstantCost(61462),
            },
            trace: CostingFun {
                mem: TwoArguments::ConstantCost(32),
                cpu: TwoArguments::ConstantCost(59498),
            },
            fst_pair: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(141895),
            },
            snd_pair: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(141992),
            },
            choose_list: CostingFun {
                mem: ThreeArguments::ConstantCost(32),
                cpu: ThreeArguments::ConstantCost(132994),
            },
            mk_cons: CostingFun {
                mem: TwoArguments::ConstantCost(32),
                cpu: TwoArguments::ConstantCost(72362),
            },
            head_list: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(83150),
            },
            tail_list: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(81663),
            },
            null_list: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(74433),
            },
            choose_data: CostingFun {
                mem: SixArguments::ConstantCost(32),
                cpu: SixArguments::ConstantCost(94375),
            },
            constr_data: CostingFun {
                mem: TwoArguments::ConstantCost(32),
                cpu: TwoArguments::ConstantCost(22151),
            },
            map_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(68246),
            },
            list_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(33852),
            },
            i_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(15299),
            },
            b_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(11183),
            },
            un_constr_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(24588),
            },
            un_map_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(24623),
            },
            un_list_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(25933),
            },
            un_i_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(20744),
            },
            un_b_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(20142),
            },
            equals_data: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 898148,
                    slope: 27279,
                }),
            },
            mk_pair_data: CostingFun {
                mem: TwoArguments::ConstantCost(32),
                cpu: TwoArguments::ConstantCost(11546),
            },
            mk_nil_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(7243),
            },
            mk_nil_pair_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(7391),
            },
            serialise_data: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: 0,
                    slope: 2,
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 955506,
                    slope: 213312,
                }),
            },
            blake2b_224: CostingFun {
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 207616,
                    slope: 8310,
                }),
                mem: OneArgument::ConstantCost(4),
            },
            keccak_256: CostingFun {
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 2261318,
                    slope: 64571,
                }),
                mem: OneArgument::ConstantCost(4),
            },
            bls12_381_g1_add: CostingFun {
                cpu: TwoArguments::ConstantCost(962335),
                mem: TwoArguments::ConstantCost(18),
            },
            bls12_381_g1_neg: CostingFun {
                cpu: OneArgument::ConstantCost(267929),
                mem: OneArgument::ConstantCost(18),
            },
            bls12_381_g1_scalar_mul: CostingFun {
                cpu: TwoArguments::LinearInX(LinearSize {
                    intercept: 76433006,
                    slope: 8868,
                }),
                mem: TwoArguments::ConstantCost(18),
            },
            bls12_381_g1_equal: CostingFun {
                cpu: TwoArguments::ConstantCost(442008),
                mem: TwoArguments::ConstantCost(1),
            },
            bls12_381_g1_compress: CostingFun {
                cpu: OneArgument::ConstantCost(2780678),
                mem: OneArgument::ConstantCost(6),
            },
            bls12_381_g1_uncompress: CostingFun {
                cpu: OneArgument::ConstantCost(52948122),
                mem: OneArgument::ConstantCost(18),
            },
            bls12_381_g1_hash_to_group: CostingFun {
                cpu: TwoArguments::LinearInX(LinearSize {
                    intercept: 52538055,
                    slope: 3756,
                }),
                mem: TwoArguments::ConstantCost(18),
            },
            bls12_381_g2_add: CostingFun {
                cpu: TwoArguments::ConstantCost(1995836),
                mem: TwoArguments::ConstantCost(36),
            },
            bls12_381_g2_neg: CostingFun {
                cpu: OneArgument::ConstantCost(284546),
                mem: OneArgument::ConstantCost(36),
            },
            bls12_381_g2_scalar_mul: CostingFun {
                cpu: TwoArguments::LinearInX(LinearSize {
                    intercept: 158221314,
                    slope: 26549,
                }),
                mem: TwoArguments::ConstantCost(36),
            },
            bls12_381_g2_equal: CostingFun {
                cpu: TwoArguments::ConstantCost(901022),
                mem: TwoArguments::ConstantCost(1),
            },
            bls12_381_g2_compress: CostingFun {
                cpu: OneArgument::ConstantCost(3227919),
                mem: OneArgument::ConstantCost(12),
            },
            bls12_381_g2_uncompress: CostingFun {
                cpu: OneArgument::ConstantCost(74698472),
                mem: OneArgument::ConstantCost(36),
            },
            bls12_381_g2_hash_to_group: CostingFun {
                cpu: TwoArguments::LinearInX(LinearSize {
                    intercept: 166917843,
                    slope: 4307,
                }),
                mem: TwoArguments::ConstantCost(36),
            },
            bls12_381_miller_loop: CostingFun {
                cpu: TwoArguments::ConstantCost(254006273),
                mem: TwoArguments::ConstantCost(72),
            },
            bls12_381_mul_ml_result: CostingFun {
                cpu: TwoArguments::ConstantCost(2174038),
                mem: TwoArguments::ConstantCost(72),
            },
            bls12_381_final_verify: CostingFun {
                cpu: TwoArguments::ConstantCost(333849714),
                mem: TwoArguments::ConstantCost(1),
            },
            integer_to_byte_string: CostingFun {
                cpu: ThreeArguments::QuadraticInZ(QuadraticFunction {
                    coeff_0: 1293828,
                    coeff_1: 28716,
                    coeff_2: 63,
                }),
                mem: ThreeArguments::LiteralInYorLinearInZ(LinearSize {
                    intercept: 0,
                    slope: 1,
                }),
            },
            byte_string_to_integer: CostingFun {
                cpu: TwoArguments::QuadraticInY(QuadraticFunction {
                    coeff_0: 1006041,
                    coeff_1: 43623,
                    coeff_2: 251,
                }),
                mem: TwoArguments::LinearInY(LinearSize {
                    intercept: 0,
                    slope: 1,
                }),
            },
            and_byte_string: CostingFun {
                cpu: ThreeArguments::LinearInYandZ(TwoVariableLinearSize {
                    intercept: 100181,
                    slope1: 726,
                    slope2: 719,
                }),
                mem: ThreeArguments::LinearInMaxYZ(LinearSize {
                    intercept: 0,
                    slope: 1,
                }),
            },
            or_byte_string: CostingFun {
                cpu: ThreeArguments::LinearInYandZ(TwoVariableLinearSize {
                    intercept: 100181,
                    slope1: 726,
                    slope2: 719,
                }),
                mem: ThreeArguments::LinearInMaxYZ(LinearSize {
                    intercept: 0,
                    slope: 1,
                }),
            },
            xor_byte_string: CostingFun {
                cpu: ThreeArguments::LinearInYandZ(TwoVariableLinearSize {
                    intercept: 100181,
                    slope1: 726,
                    slope2: 719,
                }),
                mem: ThreeArguments::LinearInMaxYZ(LinearSize {
                    intercept: 0,
                    slope: 1,
                }),
            },
            complement_byte_string: CostingFun {
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 107878,
                    slope: 680,
                }),
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: 0,
                    slope: 1,
                }),
            },
            read_bit: CostingFun {
                cpu: TwoArguments::ConstantCost(95336),
                mem: TwoArguments::ConstantCost(1),
            },
            write_bits: CostingFun {
                cpu: ThreeArguments::LinearInY(LinearSize {
                    intercept: 281145,
                    slope: 18848,
                }),
                mem: ThreeArguments::LinearInX(LinearSize {
                    intercept: 0,
                    slope: 1,
                }),
            },
            replicate_byte: CostingFun {
                cpu: TwoArguments::LinearInX(LinearSize {
                    intercept: 180194,
                    slope: 159,
                }),
                mem: TwoArguments::LinearInX(LinearSize {
                    intercept: 1,
                    slope: 1,
                }),
            },
            shift_byte_string: CostingFun {
                cpu: TwoArguments::LinearInX(LinearSize {
                    intercept: 158519,
                    slope: 8942,
                }),
                mem: TwoArguments::LinearInX(LinearSize {
                    intercept: 0,
                    slope: 1,
                }),
            },
            rotate_byte_string: CostingFun {
                cpu: TwoArguments::LinearInX(LinearSize {
                    intercept: 159378,
                    slope: 8813,
                }),
                mem: TwoArguments::LinearInX(LinearSize {
                    intercept: 0,
                    slope: 1,
                }),
            },
            count_set_bits: CostingFun {
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 107490,
                    slope: 3298,
                }),
                mem: OneArgument::ConstantCost(1),
            },
            find_first_set_bit: CostingFun {
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 106057,
                    slope: 655,
                }),
                mem: OneArgument::ConstantCost(1),
            },
            ripemd_160: CostingFun {
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 1964219,
                    slope: 24520,
                }),
                mem: OneArgument::ConstantCost(3),
            },
            // Not yet properly costed
            exp_mod_int: CostingFun {
                cpu: ThreeArguments::ConstantCost(30000000000),
                mem: ThreeArguments::ConstantCost(30000000000),
            },
        }
    }
}

impl Default for BuiltinCosts {
    fn default() -> Self {
        BuiltinCosts::v3()
    }
}

impl BuiltinCosts {
    pub fn to_ex_budget(&self, fun: DefaultFunction, args: &[Value]) -> Result<ExBudget, Error> {
        Ok(match fun {
            DefaultFunction::AddInteger => ExBudget {
                mem: self
                    .add_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .add_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::SubtractInteger => ExBudget {
                mem: self
                    .subtract_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .subtract_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::MultiplyInteger => ExBudget {
                mem: self
                    .multiply_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .multiply_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::DivideInteger => ExBudget {
                mem: self
                    .divide_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .divide_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::QuotientInteger => ExBudget {
                mem: self
                    .quotient_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .quotient_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::RemainderInteger => ExBudget {
                mem: self
                    .remainder_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .remainder_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::ModInteger => ExBudget {
                mem: self
                    .mod_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .mod_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::EqualsInteger => ExBudget {
                mem: self
                    .equals_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .equals_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::LessThanInteger => ExBudget {
                mem: self
                    .less_than_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .less_than_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::LessThanEqualsInteger => ExBudget {
                mem: self
                    .less_than_equals_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .less_than_equals_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::AppendByteString => ExBudget {
                mem: self
                    .append_byte_string
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .append_byte_string
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::ConsByteString => ExBudget {
                mem: self
                    .cons_byte_string
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .cons_byte_string
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::SliceByteString => ExBudget {
                mem: self.slice_byte_string.mem.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
                cpu: self.slice_byte_string.cpu.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
            },
            DefaultFunction::LengthOfByteString => ExBudget {
                mem: self.length_of_byte_string.mem.cost(args[0].to_ex_mem()),
                cpu: self.length_of_byte_string.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::IndexByteString => ExBudget {
                mem: self
                    .index_byte_string
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .index_byte_string
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::EqualsByteString => ExBudget {
                mem: self
                    .equals_byte_string
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .equals_byte_string
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::LessThanByteString => ExBudget {
                mem: self
                    .less_than_byte_string
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .less_than_byte_string
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::LessThanEqualsByteString => ExBudget {
                mem: self
                    .less_than_equals_byte_string
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .less_than_equals_byte_string
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::Sha2_256 => ExBudget {
                mem: self.sha2_256.mem.cost(args[0].to_ex_mem()),
                cpu: self.sha2_256.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::Sha3_256 => ExBudget {
                mem: self.sha3_256.mem.cost(args[0].to_ex_mem()),
                cpu: self.sha3_256.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::Blake2b_256 => ExBudget {
                mem: self.blake2b_256.mem.cost(args[0].to_ex_mem()),
                cpu: self.blake2b_256.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::VerifyEd25519Signature => ExBudget {
                mem: self.verify_ed25519_signature.mem.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
                cpu: self.verify_ed25519_signature.cpu.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
            },
            DefaultFunction::VerifyEcdsaSecp256k1Signature => ExBudget {
                mem: self.verify_ecdsa_secp256k1_signature.mem.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
                cpu: self.verify_ecdsa_secp256k1_signature.cpu.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
            },
            DefaultFunction::VerifySchnorrSecp256k1Signature => ExBudget {
                mem: self.verify_schnorr_secp256k1_signature.mem.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
                cpu: self.verify_schnorr_secp256k1_signature.cpu.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
            },
            DefaultFunction::AppendString => ExBudget {
                mem: self
                    .append_string
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .append_string
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::EqualsString => ExBudget {
                mem: self
                    .equals_string
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .equals_string
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::EncodeUtf8 => ExBudget {
                mem: self.encode_utf8.mem.cost(args[0].to_ex_mem()),
                cpu: self.encode_utf8.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::DecodeUtf8 => ExBudget {
                mem: self.decode_utf8.mem.cost(args[0].to_ex_mem()),
                cpu: self.decode_utf8.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::IfThenElse => ExBudget {
                mem: self.if_then_else.mem.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
                cpu: self.if_then_else.cpu.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
            },
            DefaultFunction::ChooseUnit => ExBudget {
                mem: self
                    .choose_unit
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .choose_unit
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::Trace => ExBudget {
                mem: self
                    .trace
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .trace
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::FstPair => ExBudget {
                mem: self.fst_pair.mem.cost(args[0].to_ex_mem()),
                cpu: self.fst_pair.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::SndPair => ExBudget {
                mem: self.snd_pair.mem.cost(args[0].to_ex_mem()),
                cpu: self.snd_pair.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::ChooseList => ExBudget {
                mem: self.choose_list.mem.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
                cpu: self.choose_list.cpu.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
            },
            DefaultFunction::MkCons => ExBudget {
                mem: self
                    .mk_cons
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .mk_cons
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::HeadList => ExBudget {
                mem: self.head_list.mem.cost(args[0].to_ex_mem()),
                cpu: self.head_list.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::TailList => ExBudget {
                mem: self.tail_list.mem.cost(args[0].to_ex_mem()),
                cpu: self.tail_list.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::NullList => ExBudget {
                mem: self.null_list.mem.cost(args[0].to_ex_mem()),
                cpu: self.null_list.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::ChooseData => ExBudget {
                mem: self.choose_data.mem.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                    args[3].to_ex_mem(),
                    args[4].to_ex_mem(),
                    args[5].to_ex_mem(),
                ),
                cpu: self.choose_data.cpu.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                    args[3].to_ex_mem(),
                    args[4].to_ex_mem(),
                    args[5].to_ex_mem(),
                ),
            },
            DefaultFunction::ConstrData => ExBudget {
                mem: self
                    .constr_data
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .constr_data
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::MapData => ExBudget {
                mem: self.map_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.map_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::ListData => ExBudget {
                mem: self.list_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.list_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::IData => ExBudget {
                mem: self.i_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.i_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::BData => ExBudget {
                mem: self.b_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.b_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::UnConstrData => ExBudget {
                mem: self.un_constr_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.un_constr_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::UnMapData => ExBudget {
                mem: self.un_map_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.un_map_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::UnListData => ExBudget {
                mem: self.un_list_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.un_list_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::UnIData => ExBudget {
                mem: self.un_i_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.un_i_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::UnBData => ExBudget {
                mem: self.un_b_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.un_b_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::EqualsData => ExBudget {
                mem: self
                    .equals_data
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .equals_data
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::SerialiseData => ExBudget {
                mem: self.serialise_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.serialise_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::MkPairData => ExBudget {
                mem: self
                    .mk_pair_data
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .mk_pair_data
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::MkNilData => ExBudget {
                mem: self.mk_nil_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.mk_nil_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::MkNilPairData => ExBudget {
                mem: self.mk_nil_pair_data.mem.cost(args[0].to_ex_mem()),
                cpu: self.mk_nil_pair_data.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::Keccak_256 => ExBudget {
                mem: self.keccak_256.mem.cost(args[0].to_ex_mem()),
                cpu: self.keccak_256.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::Blake2b_224 => ExBudget {
                mem: self.blake2b_224.mem.cost(args[0].to_ex_mem()),
                cpu: self.blake2b_224.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::Bls12_381_G1_Add => ExBudget {
                mem: self
                    .bls12_381_g1_add
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .bls12_381_g1_add
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::Bls12_381_G1_Neg => ExBudget {
                mem: self.bls12_381_g1_neg.mem.cost(args[0].to_ex_mem()),
                cpu: self.bls12_381_g1_neg.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::Bls12_381_G1_ScalarMul => ExBudget {
                mem: self
                    .bls12_381_g1_scalar_mul
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .bls12_381_g1_scalar_mul
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::Bls12_381_G1_Equal => ExBudget {
                mem: self
                    .bls12_381_g1_equal
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .bls12_381_g1_equal
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::Bls12_381_G1_Compress => ExBudget {
                mem: self.bls12_381_g1_compress.mem.cost(args[0].to_ex_mem()),
                cpu: self.bls12_381_g1_compress.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::Bls12_381_G1_Uncompress => ExBudget {
                mem: self.bls12_381_g1_uncompress.mem.cost(args[0].to_ex_mem()),
                cpu: self.bls12_381_g1_uncompress.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::Bls12_381_G1_HashToGroup => ExBudget {
                mem: self
                    .bls12_381_g1_hash_to_group
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .bls12_381_g1_hash_to_group
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::Bls12_381_G2_Add => ExBudget {
                mem: self
                    .bls12_381_g2_add
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .bls12_381_g2_add
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::Bls12_381_G2_Neg => ExBudget {
                mem: self.bls12_381_g2_neg.mem.cost(args[0].to_ex_mem()),
                cpu: self.bls12_381_g2_neg.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::Bls12_381_G2_ScalarMul => ExBudget {
                mem: self
                    .bls12_381_g2_scalar_mul
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .bls12_381_g2_scalar_mul
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::Bls12_381_G2_Equal => ExBudget {
                mem: self
                    .bls12_381_g2_equal
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .bls12_381_g2_equal
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::Bls12_381_G2_Compress => ExBudget {
                mem: self.bls12_381_g2_compress.mem.cost(args[0].to_ex_mem()),
                cpu: self.bls12_381_g2_compress.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::Bls12_381_G2_Uncompress => ExBudget {
                mem: self.bls12_381_g2_uncompress.mem.cost(args[0].to_ex_mem()),
                cpu: self.bls12_381_g2_uncompress.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::Bls12_381_G2_HashToGroup => ExBudget {
                mem: self
                    .bls12_381_g2_hash_to_group
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .bls12_381_g2_hash_to_group
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::Bls12_381_MillerLoop => ExBudget {
                mem: self
                    .bls12_381_miller_loop
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .bls12_381_miller_loop
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::Bls12_381_MulMlResult => ExBudget {
                mem: self
                    .bls12_381_mul_ml_result
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .bls12_381_mul_ml_result
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::Bls12_381_FinalVerify => ExBudget {
                mem: self
                    .bls12_381_final_verify
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .bls12_381_final_verify
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            d @ DefaultFunction::IntegerToByteString => {
                let size = args[1].cost_as_size(d)?;

                ExBudget {
                    mem: self.integer_to_byte_string.mem.cost(
                        args[0].to_ex_mem(),
                        size,
                        args[2].to_ex_mem(),
                    ),
                    cpu: self.integer_to_byte_string.cpu.cost(
                        args[0].to_ex_mem(),
                        size,
                        args[2].to_ex_mem(),
                    ),
                }
            }
            DefaultFunction::ByteStringToInteger => ExBudget {
                mem: self
                    .byte_string_to_integer
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .byte_string_to_integer
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::AndByteString => ExBudget {
                mem: self.and_byte_string.mem.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
                cpu: self.and_byte_string.cpu.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
            },
            DefaultFunction::OrByteString => ExBudget {
                mem: self.or_byte_string.mem.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
                cpu: self.or_byte_string.cpu.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
            },
            DefaultFunction::XorByteString => ExBudget {
                mem: self.xor_byte_string.mem.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
                cpu: self.xor_byte_string.cpu.cost(
                    args[0].to_ex_mem(),
                    args[1].to_ex_mem(),
                    args[2].to_ex_mem(),
                ),
            },
            DefaultFunction::ComplementByteString => ExBudget {
                mem: self.complement_byte_string.mem.cost(args[0].to_ex_mem()),
                cpu: self.complement_byte_string.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::ReadBit => ExBudget {
                mem: self
                    .read_bit
                    .mem
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
                cpu: self
                    .read_bit
                    .cpu
                    .cost(args[0].to_ex_mem(), args[1].to_ex_mem()),
            },
            DefaultFunction::WriteBits => {
                let list = args[1].unwrap_list().unwrap();

                ExBudget {
                    mem: self.write_bits.mem.cost(
                        args[0].to_ex_mem(),
                        list.1.len() as i64,
                        args[2].to_ex_mem(),
                    ),
                    cpu: self.write_bits.cpu.cost(
                        args[0].to_ex_mem(),
                        list.1.len() as i64,
                        args[2].to_ex_mem(),
                    ),
                }
            }
            d @ DefaultFunction::ReplicateByte => {
                let size = args[0].cost_as_size(d)?;

                ExBudget {
                    mem: self.replicate_byte.mem.cost(size, args[1].to_ex_mem()),
                    cpu: self.replicate_byte.cpu.cost(size, args[1].to_ex_mem()),
                }
            }
            DefaultFunction::ShiftByteString => {
                let literal = args[1].unwrap_integer()?;

                let arg1: i64 = u64::try_from(literal.abs())
                    .unwrap()
                    .try_into()
                    .unwrap_or(i64::MAX);

                ExBudget {
                    mem: self.shift_byte_string.mem.cost(args[0].to_ex_mem(), arg1),
                    cpu: self.shift_byte_string.cpu.cost(args[0].to_ex_mem(), arg1),
                }
            }
            DefaultFunction::RotateByteString => {
                let literal = args[1].unwrap_integer()?;

                let arg1: i64 = u64::try_from(literal.abs())
                    .unwrap()
                    .try_into()
                    .unwrap_or(i64::MAX);

                ExBudget {
                    mem: self.rotate_byte_string.mem.cost(args[0].to_ex_mem(), arg1),
                    cpu: self.rotate_byte_string.cpu.cost(args[0].to_ex_mem(), arg1),
                }
            }
            DefaultFunction::CountSetBits => ExBudget {
                mem: self.count_set_bits.mem.cost(args[0].to_ex_mem()),
                cpu: self.count_set_bits.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::FindFirstSetBit => ExBudget {
                mem: self.find_first_set_bit.mem.cost(args[0].to_ex_mem()),
                cpu: self.find_first_set_bit.cpu.cost(args[0].to_ex_mem()),
            },
            DefaultFunction::Ripemd_160 => ExBudget {
                mem: self.ripemd_160.mem.cost(args[0].to_ex_mem()),
                cpu: self.ripemd_160.cpu.cost(args[0].to_ex_mem()),
            },
            // DefaultFunction::ExpModInteger => {
            //     let arg3 = args[2].unwrap_integer()?;
            //     if arg3.lt(&(0.into())) {
            //         return Err(Error::OutsideNaturalBounds(arg3.clone()));
            //     }

            //     let arg3_exmem = if *arg3 == 0.into() {
            //         1
            //     } else {
            //         (integer_log2(arg3.abs()) / 64) + 1
            //     };

            //     ExBudget {
            //         mem: self.exp_mod_int.mem.cost(
            //             args[0].to_ex_mem(),
            //             args[1].to_ex_mem(),
            //             arg3_exmem,
            //         ),
            //         cpu: self.exp_mod_int.cpu.cost(
            //             args[0].to_ex_mem(),
            //             args[1].to_ex_mem(),
            //             arg3_exmem,
            //         ),
            //     }
            // }
        })
    }
}

pub fn initialize_cost_model(version: &Language, costs: &[i64]) -> CostModel {
    let cost_map: HashMap<&str, i64> = match version {
        Language::PlutusV1 => {
            hashmap! {
                "add_integer-cpu-arguments-intercept" => costs[0],
                "add_integer-cpu-arguments-slope" => costs[1],
                "add_integer-mem-arguments-intercept" => costs[2],
                "add_integer-mem-arguments-slope" => costs[3],
                "append_byte_string-cpu-arguments-intercept" => costs[4],
                "append_byte_string-cpu-arguments-slope" => costs[5],
                "append_byte_string-mem-arguments-intercept" => costs[6],
                "append_byte_string-mem-arguments-slope" => costs[7],
                "append_string-cpu-arguments-intercept" => costs[8],
                "append_string-cpu-arguments-slope" => costs[9],
                "append_string-mem-arguments-intercept" => costs[10],
                "append_string-mem-arguments-slope" => costs[11],
                "b_data-cpu-arguments" => costs[12],
                "b_data-mem-arguments" => costs[13],
                "blake2b_256-cpu-arguments-intercept" => costs[14],
                "blake2b_256-cpu-arguments-slope" => costs[15],
                "blake2b_256-mem-arguments" => costs[16],
                "cek_apply_cost-exBudgetCPU" => costs[17],
                "cek_apply_cost-exBudgetmem" => costs[18],
                "cek_builtin_cost-exBudgetCPU" => costs[19],
                "cek_builtin_cost-exBudgetmem" => costs[20],
                "cek_const_cost-exBudgetCPU" => costs[21],
                "cek_const_cost-exBudgetmem" => costs[22],
                "cek_delay_cost-exBudgetCPU" => costs[23],
                "cek_delay_cost-exBudgetmem" => costs[24],
                "cek_force_cost-exBudgetCPU" => costs[25],
                "cek_force_cost-exBudgetmem" => costs[26],
                "cek_lam_cost-exBudgetCPU" => costs[27],
                "cek_lam_cost-exBudgetmem" => costs[28],
                "cek_startup_cost-exBudgetCPU" => costs[29],
                "cek_startup_cost-exBudgetmem" => costs[30],
                "cek_var_cost-exBudgetCPU" => costs[31],
                "cek_var_cost-exBudgetmem" => costs[32],
                "choose_data-cpu-arguments" => costs[33],
                "choose_data-mem-arguments" => costs[34],
                "choose_list-cpu-arguments" => costs[35],
                "choose_list-mem-arguments" => costs[36],
                "choose_unit-cpu-arguments" => costs[37],
                "choose_unit-mem-arguments" => costs[38],
                "cons_byte_string-cpu-arguments-intercept" => costs[39],
                "cons_byte_string-cpu-arguments-slope" => costs[40],
                "cons_byte_string-mem-arguments-intercept" => costs[41],
                "cons_byte_string-mem-arguments-slope" => costs[42],
                "constr_data-cpu-arguments" => costs[43],
                "constr_data-mem-arguments" => costs[44],
                "decode_utf8-cpu-arguments-intercept" => costs[45],
                "decode_utf8-cpu-arguments-slope" => costs[46],
                "decode_utf8-mem-arguments-intercept" => costs[47],
                "decode_utf8-mem-arguments-slope" => costs[48],
                "divide_integer-cpu-arguments-constant" => costs[49],
                "divide_integer-cpu-arguments-model-arguments-intercept" => costs[50],
                "divide_integer-cpu-arguments-model-arguments-slope" => costs[51],
                "divide_integer-mem-arguments-intercept" => costs[52],
                "divide_integer-mem-arguments-minimum" => costs[53],
                "divide_integer-mem-arguments-slope" => costs[54],
                "encode_utf8-cpu-arguments-intercept" => costs[55],
                "encode_utf8-cpu-arguments-slope" => costs[56],
                "encode_utf8-mem-arguments-intercept" => costs[57],
                "encode_utf8-mem-arguments-slope" => costs[58],
                "equals_byte_string-cpu-arguments-constant" => costs[59],
                "equals_byte_string-cpu-arguments-intercept" => costs[60],
                "equals_byte_string-cpu-arguments-slope" => costs[61],
                "equals_byte_string-mem-arguments" => costs[62],
                "equals_data-cpu-arguments-intercept" => costs[63],
                "equals_data-cpu-arguments-slope" => costs[64],
                "equals_data-mem-arguments" => costs[65],
                "equals_integer-cpu-arguments-intercept" => costs[66],
                "equals_integer-cpu-arguments-slope" => costs[67],
                "equals_integer-mem-arguments" => costs[68],
                "equals_string-cpu-arguments-constant" => costs[69],
                "equals_string-cpu-arguments-intercept" => costs[70],
                "equals_string-cpu-arguments-slope" => costs[71],
                "equals_string-mem-arguments" => costs[72],
                "fst_pair-cpu-arguments" => costs[73],
                "fst_pair-mem-arguments" => costs[74],
                "head_list-cpu-arguments" => costs[75],
                "head_list-mem-arguments" => costs[76],
                "i_data-cpu-arguments" => costs[77],
                "i_data-mem-arguments" => costs[78],
                "if_then_else-cpu-arguments" => costs[79],
                "if_then_else-mem-arguments" => costs[80],
                "index_byte_string-cpu-arguments" => costs[81],
                "index_byte_string-mem-arguments" => costs[82],
                "length_of_byte_string-cpu-arguments" => costs[83],
                "length_of_byte_string-mem-arguments" => costs[84],
                "less_than_byte_string-cpu-arguments-intercept" => costs[85],
                "less_than_byte_string-cpu-arguments-slope" => costs[86],
                "less_than_byte_string-mem-arguments" => costs[87],
                "less_than_equals_byte_string-cpu-arguments-intercept" => costs[88],
                "less_than_equals_byte_string-cpu-arguments-slope" => costs[89],
                "less_than_equals_byte_string-mem-arguments" => costs[90],
                "less_than_equals_integer-cpu-arguments-intercept" => costs[91],
                "less_than_equals_integer-cpu-arguments-slope" => costs[92],
                "less_than_equals_integer-mem-arguments" => costs[93],
                "less_than_integer-cpu-arguments-intercept" => costs[94],
                "less_than_integer-cpu-arguments-slope" => costs[95],
                "less_than_integer-mem-arguments" => costs[96],
                "list_data-cpu-arguments" => costs[97],
                "list_data-mem-arguments" => costs[98],
                "map_data-cpu-arguments" => costs[99],
                "map_data-mem-arguments" => costs[100],
                "mk_cons-cpu-arguments" => costs[101],
                "mk_cons-mem-arguments" => costs[102],
                "mk_nil_data-cpu-arguments" => costs[103],
                "mk_nil_data-mem-arguments" => costs[104],
                "mk_nil_pair_data-cpu-arguments" => costs[105],
                "mk_nil_pair_data-mem-arguments" => costs[106],
                "mk_pair_data-cpu-arguments" => costs[107],
                "mk_pair_data-mem-arguments" => costs[108],
                "mod_integer-cpu-arguments-constant" => costs[109],
                "mod_integer-cpu-arguments-model-arguments-intercept" => costs[110],
                "mod_integer-cpu-arguments-model-arguments-slope" => costs[111],
                "mod_integer-mem-arguments-intercept" => costs[112],
                "mod_integer-mem-arguments-minimum" => costs[113],
                "mod_integer-mem-arguments-slope" => costs[114],
                "multiply_integer-cpu-arguments-intercept" => costs[115],
                "multiply_integer-cpu-arguments-slope" => costs[116],
                "multiply_integer-mem-arguments-intercept" => costs[117],
                "multiply_integer-mem-arguments-slope" => costs[118],
                "null_list-cpu-arguments" => costs[119],
                "null_list-mem-arguments" => costs[120],
                "quotient_integer-cpu-arguments-constant" => costs[121],
                "quotient_integer-cpu-arguments-model-arguments-intercept" => costs[122],
                "quotient_integer-cpu-arguments-model-arguments-slope" => costs[123],
                "quotient_integer-mem-arguments-intercept" => costs[124],
                "quotient_integer-mem-arguments-minimum" => costs[125],
                "quotient_integer-mem-arguments-slope" => costs[126],
                "remainder_integer-cpu-arguments-constant" => costs[127],
                "remainder_integer-cpu-arguments-model-arguments-intercept" => costs[128],
                "remainder_integer-cpu-arguments-model-arguments-slope" => costs[129],
                "remainder_integer-mem-arguments-intercept" => costs[130],
                "remainder_integer-mem-arguments-minimum" => costs[131],
                "remainder_integer-mem-arguments-slope" => costs[132],
                "sha2_256-cpu-arguments-intercept" => costs[133],
                "sha2_256-cpu-arguments-slope" => costs[134],
                "sha2_256-mem-arguments" => costs[135],
                "sha3_256-cpu-arguments-intercept" => costs[136],
                "sha3_256-cpu-arguments-slope" => costs[137],
                "sha3_256-mem-arguments" => costs[138],
                "slice_byte_string-cpu-arguments-intercept" => costs[139],
                "slice_byte_string-cpu-arguments-slope" => costs[140],
                "slice_byte_string-mem-arguments-intercept" => costs[141],
                "slice_byte_string-mem-arguments-slope" => costs[142],
                "snd_pair-cpu-arguments" => costs[143],
                "snd_pair-mem-arguments" => costs[144],
                "subtract_integer-cpu-arguments-intercept" => costs[145],
                "subtract_integer-cpu-arguments-slope" => costs[146],
                "subtract_integer-mem-arguments-intercept" => costs[147],
                "subtract_integer-mem-arguments-slope" => costs[148],
                "tail_list-cpu-arguments" => costs[149],
                "tail_list-mem-arguments" => costs[150],
                "trace-cpu-arguments" => costs[151],
                "trace-mem-arguments" => costs[152],
                "un_b_data-cpu-arguments" => costs[153],
                "un_b_data-mem-arguments" => costs[154],
                "un_constr_data-cpu-arguments" => costs[155],
                "un_constr_data-mem-arguments" => costs[156],
                "un_i_data-cpu-arguments" => costs[157],
                "un_i_data-mem-arguments" => costs[158],
                "un_list_data-cpu-arguments" => costs[159],
                "un_list_data-mem-arguments" => costs[160],
                "un_map_data-cpu-arguments" => costs[161],
                "un_map_data-mem-arguments" => costs[162],
                "verify_ed25519_signature-cpu-arguments-intercept" => costs[163],
                "verify_ed25519_signature-cpu-arguments-slope" => costs[164],
                "verify_ed25519_signature-mem-arguments" => costs[165]
            }
        }
        Language::PlutusV2 => {
            hashmap! {
                "add_integer-cpu-arguments-intercept"=> costs[0],
                "add_integer-cpu-arguments-slope"=> costs[1],
                "add_integer-mem-arguments-intercept"=> costs[2],
                "add_integer-mem-arguments-slope"=> costs[3],
                "append_byte_string-cpu-arguments-intercept"=> costs[4],
                "append_byte_string-cpu-arguments-slope"=> costs[5],
                "append_byte_string-mem-arguments-intercept"=> costs[6],
                "append_byte_string-mem-arguments-slope"=> costs[7],
                "append_string-cpu-arguments-intercept"=> costs[8],
                "append_string-cpu-arguments-slope"=> costs[9],
                "append_string-mem-arguments-intercept"=> costs[10],
                "append_string-mem-arguments-slope"=> costs[11],
                "b_data-cpu-arguments"=> costs[12],
                "b_data-mem-arguments"=> costs[13],
                "blake2b_256-cpu-arguments-intercept"=> costs[14],
                "blake2b_256-cpu-arguments-slope"=> costs[15],
                "blake2b_256-mem-arguments"=> costs[16],
                "cek_apply_cost-exBudgetCPU"=> costs[17],
                "cek_apply_cost-exBudgetmem"=> costs[18],
                "cek_builtin_cost-exBudgetCPU"=> costs[19],
                "cek_builtin_cost-exBudgetmem"=> costs[20],
                "cek_const_cost-exBudgetCPU"=> costs[21],
                "cek_const_cost-exBudgetmem"=> costs[22],
                "cek_delay_cost-exBudgetCPU"=> costs[23],
                "cek_delay_cost-exBudgetmem"=> costs[24],
                "cek_force_cost-exBudgetCPU"=> costs[25],
                "cek_force_cost-exBudgetmem"=> costs[26],
                "cek_lam_cost-exBudgetCPU"=> costs[27],
                "cek_lam_cost-exBudgetmem"=> costs[28],
                "cek_startup_cost-exBudgetCPU"=> costs[29],
                "cek_startup_cost-exBudgetmem"=> costs[30],
                "cek_var_cost-exBudgetCPU"=> costs[31],
                "cek_var_cost-exBudgetmem"=> costs[32],
                "choose_data-cpu-arguments"=> costs[33],
                "choose_data-mem-arguments"=> costs[34],
                "choose_list-cpu-arguments"=> costs[35],
                "choose_list-mem-arguments"=> costs[36],
                "choose_unit-cpu-arguments"=> costs[37],
                "choose_unit-mem-arguments"=> costs[38],
                "cons_byte_string-cpu-arguments-intercept"=> costs[39],
                "cons_byte_string-cpu-arguments-slope"=> costs[40],
                "cons_byte_string-mem-arguments-intercept"=> costs[41],
                "cons_byte_string-mem-arguments-slope"=> costs[42],
                "constr_data-cpu-arguments"=> costs[43],
                "constr_data-mem-arguments"=> costs[44],
                "decode_utf8-cpu-arguments-intercept"=> costs[45],
                "decode_utf8-cpu-arguments-slope"=> costs[46],
                "decode_utf8-mem-arguments-intercept"=> costs[47],
                "decode_utf8-mem-arguments-slope"=> costs[48],
                "divide_integer-cpu-arguments-constant"=> costs[49],
                "divide_integer-cpu-arguments-model-arguments-intercept"=> costs[50],
                "divide_integer-cpu-arguments-model-arguments-slope"=> costs[51],
                "divide_integer-mem-arguments-intercept"=> costs[52],
                "divide_integer-mem-arguments-minimum"=> costs[53],
                "divide_integer-mem-arguments-slope"=> costs[54],
                "encode_utf8-cpu-arguments-intercept"=> costs[55],
                "encode_utf8-cpu-arguments-slope"=> costs[56],
                "encode_utf8-mem-arguments-intercept"=> costs[57],
                "encode_utf8-mem-arguments-slope"=> costs[58],
                "equals_byte_string-cpu-arguments-constant"=> costs[59],
                "equals_byte_string-cpu-arguments-intercept"=> costs[60],
                "equals_byte_string-cpu-arguments-slope"=> costs[61],
                "equals_byte_string-mem-arguments"=> costs[62],
                "equals_data-cpu-arguments-intercept"=> costs[63],
                "equals_data-cpu-arguments-slope"=> costs[64],
                "equals_data-mem-arguments"=> costs[65],
                "equals_integer-cpu-arguments-intercept"=> costs[66],
                "equals_integer-cpu-arguments-slope"=> costs[67],
                "equals_integer-mem-arguments"=> costs[68],
                "equals_string-cpu-arguments-constant"=> costs[69],
                "equals_string-cpu-arguments-intercept"=> costs[70],
                "equals_string-cpu-arguments-slope"=> costs[71],
                "equals_string-mem-arguments"=> costs[72],
                "fst_pair-cpu-arguments"=> costs[73],
                "fst_pair-mem-arguments"=> costs[74],
                "head_list-cpu-arguments"=> costs[75],
                "head_list-mem-arguments"=> costs[76],
                "i_data-cpu-arguments"=> costs[77],
                "i_data-mem-arguments"=> costs[78],
                "if_then_else-cpu-arguments"=> costs[79],
                "if_then_else-mem-arguments"=> costs[80],
                "index_byte_string-cpu-arguments"=> costs[81],
                "index_byte_string-mem-arguments"=> costs[82],
                "length_of_byte_string-cpu-arguments"=> costs[83],
                "length_of_byte_string-mem-arguments"=> costs[84],
                "less_than_byte_string-cpu-arguments-intercept"=> costs[85],
                "less_than_byte_string-cpu-arguments-slope"=> costs[86],
                "less_than_byte_string-mem-arguments"=> costs[87],
                "less_than_equals_byte_string-cpu-arguments-intercept"=> costs[88],
                "less_than_equals_byte_string-cpu-arguments-slope"=> costs[89],
                "less_than_equals_byte_string-mem-arguments"=> costs[90],
                "less_than_equals_integer-cpu-arguments-intercept"=> costs[91],
                "less_than_equals_integer-cpu-arguments-slope"=> costs[92],
                "less_than_equals_integer-mem-arguments"=> costs[93],
                "less_than_integer-cpu-arguments-intercept"=> costs[94],
                "less_than_integer-cpu-arguments-slope"=> costs[95],
                "less_than_integer-mem-arguments"=> costs[96],
                "list_data-cpu-arguments"=> costs[97],
                "list_data-mem-arguments"=> costs[98],
                "map_data-cpu-arguments"=> costs[99],
                "map_data-mem-arguments"=> costs[100],
                "mk_cons-cpu-arguments"=> costs[101],
                "mk_cons-mem-arguments"=> costs[102],
                "mk_nil_data-cpu-arguments"=> costs[103],
                "mk_nil_data-mem-arguments"=> costs[104],
                "mk_nil_pair_data-cpu-arguments"=> costs[105],
                "mk_nil_pair_data-mem-arguments"=> costs[106],
                "mk_pair_data-cpu-arguments"=> costs[107],
                "mk_pair_data-mem-arguments"=> costs[108],
                "mod_integer-cpu-arguments-constant"=> costs[109],
                "mod_integer-cpu-arguments-model-arguments-intercept"=> costs[110],
                "mod_integer-cpu-arguments-model-arguments-slope"=> costs[111],
                "mod_integer-mem-arguments-intercept"=> costs[112],
                "mod_integer-mem-arguments-minimum"=> costs[113],
                "mod_integer-mem-arguments-slope"=> costs[114],
                "multiply_integer-cpu-arguments-intercept"=> costs[115],
                "multiply_integer-cpu-arguments-slope"=> costs[116],
                "multiply_integer-mem-arguments-intercept"=> costs[117],
                "multiply_integer-mem-arguments-slope"=> costs[118],
                "null_list-cpu-arguments"=> costs[119],
                "null_list-mem-arguments"=> costs[120],
                "quotient_integer-cpu-arguments-constant"=> costs[121],
                "quotient_integer-cpu-arguments-model-arguments-intercept"=> costs[122],
                "quotient_integer-cpu-arguments-model-arguments-slope"=> costs[123],
                "quotient_integer-mem-arguments-intercept"=> costs[124],
                "quotient_integer-mem-arguments-minimum"=> costs[125],
                "quotient_integer-mem-arguments-slope"=> costs[126],
                "remainder_integer-cpu-arguments-constant"=> costs[127],
                "remainder_integer-cpu-arguments-model-arguments-intercept"=> costs[128],
                "remainder_integer-cpu-arguments-model-arguments-slope"=> costs[129],
                "remainder_integer-mem-arguments-intercept"=> costs[130],
                "remainder_integer-mem-arguments-minimum"=> costs[131],
                "remainder_integer-mem-arguments-slope"=> costs[132],
                "serialise_data-cpu-arguments-intercept"=> costs[133],
                "serialise_data-cpu-arguments-slope"=> costs[134],
                "serialise_data-mem-arguments-intercept"=> costs[135],
                "serialise_data-mem-arguments-slope"=> costs[136],
                "sha2_256-cpu-arguments-intercept"=> costs[137],
                "sha2_256-cpu-arguments-slope"=> costs[138],
                "sha2_256-mem-arguments"=> costs[139],
                "sha3_256-cpu-arguments-intercept"=> costs[140],
                "sha3_256-cpu-arguments-slope"=> costs[141],
                "sha3_256-mem-arguments"=> costs[142],
                "slice_byte_string-cpu-arguments-intercept"=> costs[143],
                "slice_byte_string-cpu-arguments-slope"=> costs[144],
                "slice_byte_string-mem-arguments-intercept"=> costs[145],
                "slice_byte_string-mem-arguments-slope"=> costs[146],
                "snd_pair-cpu-arguments"=> costs[147],
                "snd_pair-mem-arguments"=> costs[148],
                "subtract_integer-cpu-arguments-intercept"=> costs[149],
                "subtract_integer-cpu-arguments-slope"=> costs[150],
                "subtract_integer-mem-arguments-intercept"=> costs[151],
                "subtract_integer-mem-arguments-slope"=> costs[152],
                "tail_list-cpu-arguments"=> costs[153],
                "tail_list-mem-arguments"=> costs[154],
                "trace-cpu-arguments"=> costs[155],
                "trace-mem-arguments"=> costs[156],
                "un_b_data-cpu-arguments"=> costs[157],
                "un_b_data-mem-arguments"=> costs[158],
                "un_constr_data-cpu-arguments"=> costs[159],
                "un_constr_data-mem-arguments"=> costs[160],
                "un_i_data-cpu-arguments"=> costs[161],
                "un_i_data-mem-arguments"=> costs[162],
                "un_list_data-cpu-arguments"=> costs[163],
                "un_list_data-mem-arguments"=> costs[164],
                "un_map_data-cpu-arguments"=> costs[165],
                "un_map_data-mem-arguments"=> costs[166],
                "verify_ecdsa_secp256k1_signature-cpu-arguments"=> costs[167],
                "verify_ecdsa_secp256k1_signature-mem-arguments"=> costs[168],
                "verify_ed25519_signature-cpu-arguments-intercept"=> costs[169],
                "verify_ed25519_signature-cpu-arguments-slope"=> costs[170],
                "verify_ed25519_signature-mem-arguments"=> costs[171],
                "verify_schnorr_secp256k1_signature-cpu-arguments-intercept"=> costs[172],
                "verify_schnorr_secp256k1_signature-cpu-arguments-slope"=> costs[173],
                "verify_schnorr_secp256k1_signature-mem-arguments"=> costs[174]
            }
        }
        Language::PlutusV3 => {
            // We can't have an assert here. This will literally break mainnet
            let mut main: HashMap<&str, i64> = hashmap! {
                "add_integer-cpu-arguments-intercept" => costs[0],
                "add_integer-cpu-arguments-slope" => costs[1],
                "add_integer-mem-arguments-intercept" => costs[2],
                "add_integer-mem-arguments-slope" => costs[3],
                "append_byte_string-cpu-arguments-intercept" => costs[4],
                "append_byte_string-cpu-arguments-slope" => costs[5],
                "append_byte_string-mem-arguments-intercept" => costs[6],
                "append_byte_string-mem-arguments-slope" => costs[7],
                "append_string-cpu-arguments-intercept" => costs[8],
                "append_string-cpu-arguments-slope" => costs[9],
                "append_string-mem-arguments-intercept" => costs[10],
                "append_string-mem-arguments-slope" => costs[11],
                "b_data-cpu-arguments" => costs[12],
                "b_data-mem-arguments" => costs[13],
                "blake2b_256-cpu-arguments-intercept" => costs[14],
                "blake2b_256-cpu-arguments-slope" => costs[15],
                "blake2b_256-mem-arguments" => costs[16],
                "cek_apply_cost-exBudgetCPU" => costs[17],
                "cek_apply_cost-exBudgetmem" => costs[18],
                "cek_builtin_cost-exBudgetCPU" => costs[19],
                "cek_builtin_cost-exBudgetmem" => costs[20],
                "cek_const_cost-exBudgetCPU" => costs[21],
                "cek_const_cost-exBudgetmem" => costs[22],
                "cek_delay_cost-exBudgetCPU" => costs[23],
                "cek_delay_cost-exBudgetmem" => costs[24],
                "cek_force_cost-exBudgetCPU" => costs[25],
                "cek_force_cost-exBudgetmem" => costs[26],
                "cek_lam_cost-exBudgetCPU" => costs[27],
                "cek_lam_cost-exBudgetmem" => costs[28],
                "cek_startup_cost-exBudgetCPU" => costs[29],
                "cek_startup_cost-exBudgetmem" => costs[30],
                "cek_var_cost-exBudgetCPU" => costs[31],
                "cek_var_cost-exBudgetmem" => costs[32],
                "choose_data-cpu-arguments" => costs[33],
                "choose_data-mem-arguments" => costs[34],
                "choose_list-cpu-arguments" => costs[35],
                "choose_list-mem-arguments" => costs[36],
                "choose_unit-cpu-arguments" => costs[37],
                "choose_unit-mem-arguments" => costs[38],
                "cons_byte_string-cpu-arguments-intercept" => costs[39],
                "cons_byte_string-cpu-arguments-slope" => costs[40],
                "cons_byte_string-mem-arguments-intercept" => costs[41],
                "cons_byte_string-mem-arguments-slope" => costs[42],
                "constr_data-cpu-arguments" => costs[43],
                "constr_data-mem-arguments" => costs[44],
                "decode_utf8-cpu-arguments-intercept" => costs[45],
                "decode_utf8-cpu-arguments-slope" => costs[46],
                "decode_utf8-mem-arguments-intercept" => costs[47],
                "decode_utf8-mem-arguments-slope" => costs[48],
                "divide_integer-cpu-arguments-constant" => costs[49],
                "divide_integer-cpu-arguments-c00" => costs[50],
                "divide_integer-cpu-arguments-c01" => costs[51],
                "divide_integer-cpu-arguments-c02" => costs[52],
                "divide_integer-cpu-arguments-c10" => costs[53],
                "divide_integer-cpu-arguments-c11" => costs[54],
                "divide_integer-cpu-arguments-c20" => costs[55],
                "divide_integer-cpu-arguments-minimum" => costs[56],
                "divide_integer-mem-arguments-intercept" => costs[57],
                "divide_integer-mem-arguments-minimum" => costs[58],
                "divide_integer-mem-arguments-slope" => costs[59],
                "encode_utf8-cpu-arguments-intercept" => costs[60],
                "encode_utf8-cpu-arguments-slope" => costs[61],
                "encode_utf8-mem-arguments-intercept" => costs[62],
                "encode_utf8-mem-arguments-slope" => costs[63],
                "equals_byte_string-cpu-arguments-constant" => costs[64],
                "equals_byte_string-cpu-arguments-intercept" => costs[65],
                "equals_byte_string-cpu-arguments-slope" => costs[66],
                "equals_byte_string-mem-arguments" => costs[67],
                "equals_data-cpu-arguments-intercept" => costs[68],
                "equals_data-cpu-arguments-slope" => costs[69],
                "equals_data-mem-arguments" => costs[70],
                "equals_integer-cpu-arguments-intercept" => costs[71],
                "equals_integer-cpu-arguments-slope" => costs[72],
                "equals_integer-mem-arguments" => costs[73],
                "equals_string-cpu-arguments-constant" => costs[74],
                "equals_string-cpu-arguments-intercept" => costs[75],
                "equals_string-cpu-arguments-slope" => costs[76],
                "equals_string-mem-arguments" => costs[77],
                "fst_pair-cpu-arguments" => costs[78],
                "fst_pair-mem-arguments" => costs[79],
                "head_list-cpu-arguments" => costs[80],
                "head_list-mem-arguments" => costs[81],
                "i_data-cpu-arguments" => costs[82],
                "i_data-mem-arguments" => costs[83],
                "if_then_else-cpu-arguments" => costs[84],
                "if_then_else-mem-arguments" => costs[85],
                "index_byte_string-cpu-arguments" => costs[86],
                "index_byte_string-mem-arguments" => costs[87],
                "length_of_byte_string-cpu-arguments" => costs[88],
                "length_of_byte_string-mem-arguments" => costs[89],
                "less_than_byte_string-cpu-arguments-intercept" => costs[90],
                "less_than_byte_string-cpu-arguments-slope" => costs[91],
                "less_than_byte_string-mem-arguments" => costs[92],
                "less_than_equals_byte_string-cpu-arguments-intercept" => costs[93],
                "less_than_equals_byte_string-cpu-arguments-slope" => costs[94],
                "less_than_equals_byte_string-mem-arguments" => costs[95],
                "less_than_equals_integer-cpu-arguments-intercept" => costs[96],
                "less_than_equals_integer-cpu-arguments-slope" => costs[97],
                "less_than_equals_integer-mem-arguments" => costs[98],
                "less_than_integer-cpu-arguments-intercept" => costs[99],
                "less_than_integer-cpu-arguments-slope" => costs[100],
                "less_than_integer-mem-arguments" => costs[101],
                "list_data-cpu-arguments" => costs[102],
                "list_data-mem-arguments" => costs[103],
                "map_data-cpu-arguments" => costs[104],
                "map_data-mem-arguments" => costs[105],
                "mk_cons-cpu-arguments" => costs[106],
                "mk_cons-mem-arguments" => costs[107],
                "mk_nil_data-cpu-arguments" => costs[108],
                "mk_nil_data-mem-arguments" => costs[109],
                "mk_nil_pair_data-cpu-arguments" => costs[110],
                "mk_nil_pair_data-mem-arguments" => costs[111],
                "mk_pair_data-cpu-arguments" => costs[112],
                "mk_pair_data-mem-arguments" => costs[113],
                "mod_integer-cpu-arguments-constant" => costs[114],
                "mod_integer-cpu-arguments-c00" => costs[115],
                "mod_integer-cpu-arguments-c01" => costs[116],
                "mod_integer-cpu-arguments-c02" => costs[117],
                "mod_integer-cpu-arguments-c10" => costs[118],
                "mod_integer-cpu-arguments-c11" => costs[119],
                "mod_integer-cpu-arguments-c20" => costs[120],
                "mod_integer-cpu-arguments-minimum" => costs[121],
                "mod_integer-mem-arguments-intercept" => costs[122],
                "mod_integer-mem-arguments-slope" => costs[123],
                "multiply_integer-cpu-arguments-intercept" => costs[124],
                "multiply_integer-cpu-arguments-slope" => costs[125],
                "multiply_integer-mem-arguments-intercept" => costs[126],
                "multiply_integer-mem-arguments-slope" => costs[127],
                "null_list-cpu-arguments" => costs[128],
                "null_list-mem-arguments" => costs[129],
                "quotient_integer-cpu-arguments-constant" => costs[130],
                "quotient_integer-cpu-arguments-c00" => costs[131],
                "quotient_integer-cpu-arguments-c01" => costs[132],
                "quotient_integer-cpu-arguments-c02" => costs[133],
                "quotient_integer-cpu-arguments-c10" => costs[134],
                "quotient_integer-cpu-arguments-c11" => costs[135],
                "quotient_integer-cpu-arguments-c20" => costs[136],
                "quotient_integer-cpu-arguments-minimum" => costs[137],
                "quotient_integer-mem-arguments-intercept" => costs[138],
                "quotient_integer-mem-arguments-minimum" => costs[139],
                "quotient_integer-mem-arguments-slope" => costs[140],
                "remainder_integer-cpu-arguments-constant" => costs[141],
                "remainder_integer-cpu-arguments-c00" => costs[142],
                "remainder_integer-cpu-arguments-c01" => costs[143],
                "remainder_integer-cpu-arguments-c02" => costs[144],
                "remainder_integer-cpu-arguments-c10" => costs[145],
                "remainder_integer-cpu-arguments-c11" => costs[146],
                "remainder_integer-cpu-arguments-c20" => costs[147],
                "remainder_integer-cpu-arguments-minimum" => costs[148],
                "remainder_integer-mem-arguments-intercept" => costs[149],
                "remainder_integer-mem-arguments-slope" => costs[150],
                "serialise_data-cpu-arguments-intercept" => costs[151],
                "serialise_data-cpu-arguments-slope" => costs[152],
                "serialise_data-mem-arguments-intercept" => costs[153],
                "serialise_data-mem-arguments-slope" => costs[154],
                "sha2_256-cpu-arguments-intercept" => costs[155],
                "sha2_256-cpu-arguments-slope" => costs[156],
                "sha2_256-mem-arguments" => costs[157],
                "sha3_256-cpu-arguments-intercept" => costs[158],
                "sha3_256-cpu-arguments-slope" => costs[159],
                "sha3_256-mem-arguments" => costs[160],
                "slice_byte_string-cpu-arguments-intercept" => costs[161],
                "slice_byte_string-cpu-arguments-slope" => costs[162],
                "slice_byte_string-mem-arguments-intercept" => costs[163],
                "slice_byte_string-mem-arguments-slope" => costs[164],
                "snd_pair-cpu-arguments" => costs[165],
                "snd_pair-mem-arguments" => costs[166],
                "subtract_integer-cpu-arguments-intercept" => costs[167],
                "subtract_integer-cpu-arguments-slope" => costs[168],
                "subtract_integer-mem-arguments-intercept" => costs[169],
                "subtract_integer-mem-arguments-slope" => costs[170],
                "tail_list-cpu-arguments" => costs[171],
                "tail_list-mem-arguments" => costs[172],
                "trace-cpu-arguments" => costs[173],
                "trace-mem-arguments" => costs[174],
                "un_b_data-cpu-arguments" => costs[175],
                "un_b_data-mem-arguments" => costs[176],
                "un_constr_data-cpu-arguments" => costs[177],
                "un_constr_data-mem-arguments" => costs[178],
                "un_i_data-cpu-arguments" => costs[179],
                "un_i_data-mem-arguments" => costs[180],
                "un_list_data-cpu-arguments" => costs[181],
                "un_list_data-mem-arguments" => costs[182],
                "un_map_data-cpu-arguments" => costs[183],
                "un_map_data-mem-arguments" => costs[184],
                "verify_ecdsa_secp256k1_signature-cpu-arguments" => costs[185],
                "verify_ecdsa_secp256k1_signature-mem-arguments" => costs[186],
                "verify_ed25519_signature-cpu-arguments-intercept" => costs[187],
                "verify_ed25519_signature-cpu-arguments-slope" => costs[188],
                "verify_ed25519_signature-mem-arguments" => costs[189],
                "verify_schnorr_secp256k1_signature-cpu-arguments-intercept" => costs[190],
                "verify_schnorr_secp256k1_signature-cpu-arguments-slope" => costs[191],
                "verify_schnorr_secp256k1_signature-mem-arguments" => costs[192],
                "cek_constr_cost-exBudgetCPU" => costs[193],
                "cek_constr_cost-exBudgetmem" => costs[194],
                "cek_case_cost-exBudgetCPU" => costs[195],
                "cek_case_cost-exBudgetmem" => costs[196],
                "bls12_381_G1_add-cpu-arguments" => costs[197],
                "bls12_381_G1_add-mem-arguments" => costs[198],
                "bls12_381_G1_compress-cpu-arguments" => costs[199],
                "bls12_381_G1_compress-mem-arguments" => costs[200],
                "bls12_381_G1_equal-cpu-arguments" => costs[201],
                "bls12_381_G1_equal-mem-arguments" => costs[202],
                "bls12_381_G1_hashToGroup-cpu-arguments-intercept" => costs[203],
                "bls12_381_G1_hashToGroup-cpu-arguments-slope" => costs[204],
                "bls12_381_G1_hashToGroup-mem-arguments" => costs[205],
                "bls12_381_G1_neg-cpu-arguments" => costs[206],
                "bls12_381_G1_neg-mem-arguments" => costs[207],
                "bls12_381_G1_scalarMul-cpu-arguments-intercept" => costs[208],
                "bls12_381_G1_scalarMul-cpu-arguments-slope" => costs[209],
                "bls12_381_G1_scalarMul-mem-arguments" => costs[210],
                "bls12_381_G1_uncompress-cpu-arguments" => costs[211],
                "bls12_381_G1_uncompress-mem-arguments" => costs[212],
                "bls12_381_G2_add-cpu-arguments" => costs[213],
                "bls12_381_G2_add-mem-arguments" => costs[214],
                "bls12_381_G2_compress-cpu-arguments" => costs[215],
                "bls12_381_G2_compress-mem-arguments" => costs[216],
                "bls12_381_G2_equal-cpu-arguments" => costs[217],
                "bls12_381_G2_equal-mem-arguments" => costs[218],
                "bls12_381_G2_hashToGroup-cpu-arguments-intercept" => costs[219],
                "bls12_381_G2_hashToGroup-cpu-arguments-slope" => costs[220],
                "bls12_381_G2_hashToGroup-mem-arguments" => costs[221],
                "bls12_381_G2_neg-cpu-arguments" => costs[222],
                "bls12_381_G2_neg-mem-arguments" => costs[223],
                "bls12_381_G2_scalarMul-cpu-arguments-intercept" => costs[224],
                "bls12_381_G2_scalarMul-cpu-arguments-slope" => costs[225],
                "bls12_381_G2_scalarMul-mem-arguments" => costs[226],
                "bls12_381_G2_uncompress-cpu-arguments" => costs[227],
                "bls12_381_G2_uncompress-mem-arguments" => costs[228],
                "bls12_381_finalVerify-cpu-arguments" => costs[229],
                "bls12_381_finalVerify-mem-arguments" => costs[230],
                "bls12_381_millerLoop-cpu-arguments" => costs[231],
                "bls12_381_millerLoop-mem-arguments" => costs[232],
                "bls12_381_mulMlResult-cpu-arguments" => costs[233],
                "bls12_381_mulMlResult-mem-arguments" => costs[234],
                "keccak_256-cpu-arguments-intercept" => costs[235],
                "keccak_256-cpu-arguments-slope" => costs[236],
                "keccak_256-mem-arguments" => costs[237],
                "blake2b_224-cpu-arguments-intercept" => costs[238],
                "blake2b_224-cpu-arguments-slope" => costs[239],
                "blake2b_224-mem-arguments-slope" => costs[240],
                "integerToByteString-cpu-arguments-c0" => costs[241],
                "integerToByteString-cpu-arguments-c1" => costs[242],
                "integerToByteString-cpu-arguments-c2" => costs[243],
                "integerToByteString-mem-arguments-intercept" => costs[244],
                "integerToByteString-mem-arguments-slope" => costs[245],
                "byteStringToInteger-cpu-arguments-c0" => costs[246],
                "byteStringToInteger-cpu-arguments-c1" => costs[247],
                "byteStringToInteger-cpu-arguments-c2" => costs[248],
                "byteStringToInteger-mem-arguments-intercept" => costs[249],
                "byteStringToInteger-mem-arguments-slope" => costs[250],
            };

            if costs.len() == 297 {
                let test = hashmap! {
                    "andByteString-cpu-arguments-intercept"=> costs[251],
                    "andByteString-cpu-arguments-slope1"=> costs[252],
                    "andByteString-cpu-arguments-slope2"=> costs[253],
                    "andByteString-memory-arguments-intercept"=> costs[254],
                    "andByteString-memory-arguments-slope"=> costs[255],
                    "orByteString-cpu-arguments-intercept"=> costs[256],
                    "orByteString-cpu-arguments-slope1"=> costs[257],
                    "orByteString-cpu-arguments-slope2"=> costs[258],
                    "orByteString-memory-arguments-intercept"=> costs[259],
                    "orByteString-memory-arguments-slope"=> costs[260],
                    "xorByteString-cpu-arguments-intercept"=> costs[261],
                    "xorByteString-cpu-arguments-slope1"=> costs[262],
                    "xorByteString-cpu-arguments-slope2"=> costs[263],
                    "xorByteString-memory-arguments-intercept"=> costs[264],
                    "xorByteString-memory-arguments-slope"=> costs[265],
                    "complementByteString-cpu-arguments-intercept"=> costs[266],
                    "complementByteString-cpu-arguments-slope"=> costs[267],
                    "complementByteString-memory-arguments-intercept"=> costs[268],
                    "complementByteString-memory-arguments-slope"=> costs[269],
                    "readBit-cpu-arguments"=> costs[270],
                    "readBit-memory-arguments"=> costs[271],
                    "writeBits-cpu-arguments-intercept"=> costs[272],
                    "writeBits-cpu-arguments-slope"=> costs[273],
                    "writeBits-memory-arguments-intercept"=> costs[274],
                    "writeBits-memory-arguments-slope"=> costs[275],
                    "replicateByte-cpu-arguments-intercept"=> costs[276],
                    "replicateByte-cpu-arguments-slope"=> costs[277],
                    "replicateByte-memory-arguments-intercept"=> costs[278],
                    "replicateByte-memory-arguments-slope"=> costs[279],
                    "shiftByteString-cpu-arguments-intercept"=> costs[280],
                    "shiftByteString-cpu-arguments-slope"=> costs[281],
                    "shiftByteString-memory-arguments-intercept"=> costs[282],
                    "shiftByteString-memory-arguments-slope"=> costs[283],
                    "rotateByteString-cpu-arguments-intercept"=> costs[284],
                    "rotateByteString-cpu-arguments-slope"=> costs[285],
                    "rotateByteString-memory-arguments-intercept"=> costs[286],
                    "rotateByteString-memory-arguments-slope"=> costs[287],
                    "countSetBits-cpu-arguments-intercept"=> costs[288],
                    "countSetBits-cpu-arguments-slope"=> costs[289],
                    "countSetBits-memory-arguments"=> costs[290],
                    "findFirstSetBit-cpu-arguments-intercept"=> costs[291],
                    "findFirstSetBit-cpu-arguments-slope"=> costs[292],
                    "findFirstSetBit-memory-arguments"=> costs[293],
                    "ripemd_160-cpu-arguments-intercept"=> costs[294],
                    "ripemd_160-cpu-arguments-slope"=> costs[295],
                    "ripemd_160-memory-arguments"=> costs[296],
                };

                Extend::extend::<HashMap<&str, i64>>(&mut main, test);
            }

            main
        }
    };

    CostModel {
        machine_costs: MachineCosts {
            startup: ExBudget {
                mem: *cost_map
                    .get("cek_startup_cost-exBudgetmem")
                    .unwrap_or(&30000000000),
                cpu: *cost_map
                    .get("cek_startup_cost-exBudgetCPU")
                    .unwrap_or(&30000000000),
            },
            var: ExBudget {
                mem: *cost_map
                    .get("cek_var_cost-exBudgetmem")
                    .unwrap_or(&30000000000),
                cpu: *cost_map
                    .get("cek_var_cost-exBudgetCPU")
                    .unwrap_or(&30000000000),
            },
            constant: ExBudget {
                mem: *cost_map
                    .get("cek_const_cost-exBudgetmem")
                    .unwrap_or(&30000000000),
                cpu: *cost_map
                    .get("cek_const_cost-exBudgetCPU")
                    .unwrap_or(&30000000000),
            },
            lambda: ExBudget {
                mem: *cost_map
                    .get("cek_lam_cost-exBudgetmem")
                    .unwrap_or(&30000000000),
                cpu: *cost_map
                    .get("cek_lam_cost-exBudgetCPU")
                    .unwrap_or(&30000000000),
            },
            delay: ExBudget {
                mem: *cost_map
                    .get("cek_delay_cost-exBudgetmem")
                    .unwrap_or(&30000000000),
                cpu: *cost_map
                    .get("cek_delay_cost-exBudgetCPU")
                    .unwrap_or(&30000000000),
            },
            force: ExBudget {
                mem: *cost_map
                    .get("cek_force_cost-exBudgetmem")
                    .unwrap_or(&30000000000),
                cpu: *cost_map
                    .get("cek_force_cost-exBudgetCPU")
                    .unwrap_or(&30000000000),
            },
            apply: ExBudget {
                mem: *cost_map
                    .get("cek_apply_cost-exBudgetmem")
                    .unwrap_or(&30000000000),
                cpu: *cost_map
                    .get("cek_apply_cost-exBudgetCPU")
                    .unwrap_or(&30000000000),
            },
            builtin: ExBudget {
                mem: *cost_map
                    .get("cek_builtin_cost-exBudgetmem")
                    .unwrap_or(&30000000000),
                cpu: *cost_map
                    .get("cek_builtin_cost-exBudgetCPU")
                    .unwrap_or(&30000000000),
            },
            constr: ExBudget {
                mem: *cost_map
                    .get("cek_constr_cost-exBudgetmem")
                    .unwrap_or(&30000000000),
                cpu: *cost_map
                    .get("cek_constr_cost-exBudgetCPU")
                    .unwrap_or(&30000000000),
            },
            case: ExBudget {
                mem: *cost_map
                    .get("cek_case_cost-exBudgetmem")
                    .unwrap_or(&30000000000),
                cpu: *cost_map
                    .get("cek_case_cost-exBudgetCPU")
                    .unwrap_or(&30000000000),
            },
        },
        builtin_costs: BuiltinCosts {
            add_integer: CostingFun {
                mem: TwoArguments::MaxSize(MaxSize {
                    intercept: *cost_map
                        .get("add_integer-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("add_integer-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
                cpu: TwoArguments::MaxSize(MaxSize {
                    intercept: *cost_map
                        .get("add_integer-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("add_integer-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            subtract_integer: CostingFun {
                mem: TwoArguments::MaxSize(MaxSize {
                    intercept: *cost_map
                        .get("subtract_integer-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("subtract_integer-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
                cpu: TwoArguments::MaxSize(MaxSize {
                    intercept: *cost_map
                        .get("subtract_integer-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("subtract_integer-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            multiply_integer: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: *cost_map
                        .get("multiply_integer-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("multiply_integer-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
                cpu: TwoArguments::MultipliedSizes(MultipliedSizes {
                    intercept: *cost_map
                        .get("multiply_integer-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("multiply_integer-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            divide_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: *cost_map
                        .get("divide_integer-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("divide_integer-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                    minimum: *cost_map
                        .get("divide_integer-mem-arguments-minimum")
                        .unwrap_or(&30000000000),
                }),
                cpu: match version {
                    Language::PlutusV1 | Language::PlutusV2 => {
                        TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                            constant: *cost_map
                                .get("divide_integer-cpu-arguments-constant")
                                .unwrap_or(&30000000000),
                            model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                                intercept: *cost_map
                                    .get("divide_integer-cpu-arguments-model-arguments-intercept")
                                    .unwrap_or(&30000000000),
                                slope: *cost_map
                                    .get("divide_integer-cpu-arguments-model-arguments-slope")
                                    .unwrap_or(&30000000000),
                            })),
                        })
                    }
                    Language::PlutusV3 => TwoArguments::ConstAboveDiagonalIntoQuadraticXAndY(
                        *cost_map
                            .get("divide_integer-cpu-arguments-constant")
                            .unwrap_or(&30000000000),
                        TwoArgumentsQuadraticFunction {
                            minimum: *cost_map
                                .get("divide_integer-cpu-arguments-minimum")
                                .unwrap_or(&30000000000),
                            coeff_00: *cost_map
                                .get("divide_integer-cpu-arguments-c00")
                                .unwrap_or(&30000000000),
                            coeff_10: *cost_map
                                .get("divide_integer-cpu-arguments-c10")
                                .unwrap_or(&30000000000),
                            coeff_01: *cost_map
                                .get("divide_integer-cpu-arguments-c01")
                                .unwrap_or(&30000000000),
                            coeff_20: *cost_map
                                .get("divide_integer-cpu-arguments-c20")
                                .unwrap_or(&30000000000),
                            coeff_11: *cost_map
                                .get("divide_integer-cpu-arguments-c11")
                                .unwrap_or(&30000000000),
                            coeff_02: *cost_map
                                .get("divide_integer-cpu-arguments-c02")
                                .unwrap_or(&30000000000),
                        },
                    ),
                },
            },
            quotient_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: *cost_map
                        .get("quotient_integer-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("quotient_integer-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                    minimum: *cost_map
                        .get("quotient_integer-mem-arguments-minimum")
                        .unwrap_or(&30000000000),
                }),
                cpu: match version {
                    Language::PlutusV1 | Language::PlutusV2 => {
                        TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                            constant: *cost_map
                                .get("quotient_integer-cpu-arguments-constant")
                                .unwrap_or(&30000000000),
                            model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                                intercept: *cost_map
                                    .get("quotient_integer-cpu-arguments-model-arguments-intercept")
                                    .unwrap_or(&30000000000),
                                slope: *cost_map
                                    .get("quotient_integer-cpu-arguments-model-arguments-slope")
                                    .unwrap_or(&30000000000),
                            })),
                        })
                    }
                    Language::PlutusV3 => TwoArguments::ConstAboveDiagonalIntoQuadraticXAndY(
                        *cost_map
                            .get("quotient_integer-cpu-arguments-constant")
                            .unwrap_or(&30000000000),
                        TwoArgumentsQuadraticFunction {
                            minimum: *cost_map
                                .get("quotient_integer-cpu-arguments-minimum")
                                .unwrap_or(&30000000000),
                            coeff_00: *cost_map
                                .get("quotient_integer-cpu-arguments-c00")
                                .unwrap_or(&30000000000),
                            coeff_10: *cost_map
                                .get("quotient_integer-cpu-arguments-c10")
                                .unwrap_or(&30000000000),
                            coeff_01: *cost_map
                                .get("quotient_integer-cpu-arguments-c01")
                                .unwrap_or(&30000000000),
                            coeff_20: *cost_map
                                .get("quotient_integer-cpu-arguments-c20")
                                .unwrap_or(&30000000000),
                            coeff_11: *cost_map
                                .get("quotient_integer-cpu-arguments-c11")
                                .unwrap_or(&30000000000),
                            coeff_02: *cost_map
                                .get("quotient_integer-cpu-arguments-c02")
                                .unwrap_or(&30000000000),
                        },
                    ),
                },
            },
            remainder_integer: CostingFun {
                mem: match version {
                    Language::PlutusV1 | Language::PlutusV2 => {
                        TwoArguments::SubtractedSizes(SubtractedSizes {
                            intercept: *cost_map
                                .get("remainder_integer-mem-arguments-intercept")
                                .unwrap_or(&30000000000),
                            slope: *cost_map
                                .get("remainder_integer-mem-arguments-slope")
                                .unwrap_or(&30000000000),
                            minimum: *cost_map
                                .get("remainder_integer-mem-arguments-minimum")
                                .unwrap_or(&30000000000),
                        })
                    }
                    Language::PlutusV3 => TwoArguments::LinearInY(LinearSize {
                        intercept: *cost_map
                            .get("remainder_integer-mem-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("remainder_integer-mem-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                },
                cpu: match version {
                    Language::PlutusV1 | Language::PlutusV2 => {
                        TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                            constant: *cost_map
                                .get("remainder_integer-cpu-arguments-constant")
                                .unwrap_or(&30000000000),
                            model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                                intercept: *cost_map
                                    .get(
                                        "remainder_integer-cpu-arguments-model-arguments-intercept",
                                    )
                                    .unwrap_or(&30000000000),
                                slope: *cost_map
                                    .get("remainder_integer-cpu-arguments-model-arguments-slope")
                                    .unwrap_or(&30000000000),
                            })),
                        })
                    }
                    Language::PlutusV3 => TwoArguments::ConstAboveDiagonalIntoQuadraticXAndY(
                        *cost_map
                            .get("remainder_integer-cpu-arguments-constant")
                            .unwrap_or(&30000000000),
                        TwoArgumentsQuadraticFunction {
                            minimum: *cost_map
                                .get("remainder_integer-cpu-arguments-minimum")
                                .unwrap_or(&30000000000),
                            coeff_00: *cost_map
                                .get("remainder_integer-cpu-arguments-c00")
                                .unwrap_or(&30000000000),
                            coeff_10: *cost_map
                                .get("remainder_integer-cpu-arguments-c10")
                                .unwrap_or(&30000000000),
                            coeff_01: *cost_map
                                .get("remainder_integer-cpu-arguments-c01")
                                .unwrap_or(&30000000000),
                            coeff_20: *cost_map
                                .get("remainder_integer-cpu-arguments-c20")
                                .unwrap_or(&30000000000),
                            coeff_11: *cost_map
                                .get("remainder_integer-cpu-arguments-c11")
                                .unwrap_or(&30000000000),
                            coeff_02: *cost_map
                                .get("remainder_integer-cpu-arguments-c02")
                                .unwrap_or(&30000000000),
                        },
                    ),
                },
            },
            mod_integer: CostingFun {
                mem: match version {
                    Language::PlutusV1 | Language::PlutusV2 => {
                        TwoArguments::SubtractedSizes(SubtractedSizes {
                            intercept: *cost_map
                                .get("mod_integer-mem-arguments-intercept")
                                .unwrap_or(&30000000000),
                            slope: *cost_map
                                .get("mod_integer-mem-arguments-slope")
                                .unwrap_or(&30000000000),
                            minimum: *cost_map
                                .get("mod_integer-mem-arguments-minimum")
                                .unwrap_or(&30000000000),
                        })
                    }
                    Language::PlutusV3 => TwoArguments::LinearInY(LinearSize {
                        intercept: *cost_map
                            .get("mod_integer-mem-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("mod_integer-mem-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                },
                cpu: match version {
                    Language::PlutusV1 | Language::PlutusV2 => {
                        TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                            constant: *cost_map
                                .get("mod_integer-cpu-arguments-constant")
                                .unwrap_or(&30000000000),
                            model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                                intercept: *cost_map
                                    .get("mod_integer-cpu-arguments-model-arguments-intercept")
                                    .unwrap_or(&30000000000),
                                slope: *cost_map
                                    .get("mod_integer-cpu-arguments-model-arguments-slope")
                                    .unwrap_or(&30000000000),
                            })),
                        })
                    }
                    Language::PlutusV3 => TwoArguments::ConstAboveDiagonalIntoQuadraticXAndY(
                        *cost_map
                            .get("mod_integer-cpu-arguments-constant")
                            .unwrap_or(&30000000000),
                        TwoArgumentsQuadraticFunction {
                            minimum: *cost_map
                                .get("mod_integer-cpu-arguments-minimum")
                                .unwrap_or(&30000000000),
                            coeff_00: *cost_map
                                .get("mod_integer-cpu-arguments-c00")
                                .unwrap_or(&30000000000),
                            coeff_10: *cost_map
                                .get("mod_integer-cpu-arguments-c10")
                                .unwrap_or(&30000000000),
                            coeff_01: *cost_map
                                .get("mod_integer-cpu-arguments-c01")
                                .unwrap_or(&30000000000),
                            coeff_20: *cost_map
                                .get("mod_integer-cpu-arguments-c20")
                                .unwrap_or(&30000000000),
                            coeff_11: *cost_map
                                .get("mod_integer-cpu-arguments-c11")
                                .unwrap_or(&30000000000),
                            coeff_02: *cost_map
                                .get("mod_integer-cpu-arguments-c02")
                                .unwrap_or(&30000000000),
                        },
                    ),
                },
            },
            equals_integer: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("equals_integer-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: *cost_map
                        .get("equals_integer-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("equals_integer-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            less_than_integer: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("less_than_integer-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: *cost_map
                        .get("less_than_integer-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("less_than_integer-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            less_than_equals_integer: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("less_than_equals_integer-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: *cost_map
                        .get("less_than_equals_integer-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("less_than_equals_integer-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            append_byte_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: *cost_map
                        .get("append_byte_string-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("append_byte_string-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
                cpu: TwoArguments::AddedSizes(AddedSizes {
                    intercept: *cost_map
                        .get("append_byte_string-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("append_byte_string-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            cons_byte_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: *cost_map
                        .get("cons_byte_string-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("cons_byte_string-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
                cpu: TwoArguments::LinearInY(LinearSize {
                    intercept: *cost_map
                        .get("cons_byte_string-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("cons_byte_string-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            slice_byte_string: CostingFun {
                mem: ThreeArguments::LinearInZ(LinearSize {
                    intercept: *cost_map
                        .get("slice_byte_string-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("slice_byte_string-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
                cpu: ThreeArguments::LinearInZ(LinearSize {
                    intercept: *cost_map
                        .get("slice_byte_string-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("slice_byte_string-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            length_of_byte_string: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("length_of_byte_string-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("length_of_byte_string-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            index_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("index_byte_string-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::ConstantCost(
                    *cost_map
                        .get("index_byte_string-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            equals_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("equals_byte_string-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::LinearOnDiagonal(ConstantOrLinear {
                    constant: *cost_map
                        .get("equals_byte_string-cpu-arguments-constant")
                        .unwrap_or(&30000000000),
                    intercept: *cost_map
                        .get("equals_byte_string-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("equals_byte_string-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            less_than_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("less_than_byte_string-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: *cost_map
                        .get("less_than_byte_string-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("less_than_byte_string-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            less_than_equals_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("less_than_equals_byte_string-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: *cost_map
                        .get("less_than_equals_byte_string-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("less_than_equals_byte_string-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            sha2_256: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("sha2_256-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: *cost_map
                        .get("sha2_256-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("sha2_256-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            sha3_256: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("sha3_256-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: *cost_map
                        .get("sha3_256-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("sha3_256-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            blake2b_256: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("blake2b_256-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: *cost_map
                        .get("blake2b_256-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("blake2b_256-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            verify_ed25519_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(
                    *cost_map
                        .get("verify_ed25519_signature-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: {
                    let sizes = LinearSize {
                        intercept: *cost_map
                            .get("verify_ed25519_signature-cpu-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("verify_ed25519_signature-cpu-arguments-slope")
                            .unwrap_or(&30000000000),
                    };

                    ThreeArguments::LinearInY(sizes)
                },
            },
            verify_ecdsa_secp256k1_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(
                    *cost_map
                        .get("verify_ecdsa_secp256k1_signature-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: ThreeArguments::ConstantCost(
                    *cost_map
                        .get("verify_ecdsa_secp256k1_signature-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            verify_schnorr_secp256k1_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(
                    *cost_map
                        .get("verify_schnorr_secp256k1_signature-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: ThreeArguments::LinearInY(LinearSize {
                    intercept: *cost_map
                        .get("verify_schnorr_secp256k1_signature-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("verify_schnorr_secp256k1_signature-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            append_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: *cost_map
                        .get("append_string-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("append_string-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
                cpu: TwoArguments::AddedSizes(AddedSizes {
                    intercept: *cost_map
                        .get("append_string-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("append_string-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            equals_string: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("equals_string-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::LinearOnDiagonal(ConstantOrLinear {
                    constant: *cost_map
                        .get("equals_string-cpu-arguments-constant")
                        .unwrap_or(&30000000000),
                    intercept: *cost_map
                        .get("equals_string-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("equals_string-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            encode_utf8: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: *cost_map
                        .get("encode_utf8-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("encode_utf8-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: *cost_map
                        .get("encode_utf8-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("encode_utf8-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            decode_utf8: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: *cost_map
                        .get("decode_utf8-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("decode_utf8-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: *cost_map
                        .get("decode_utf8-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("decode_utf8-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            if_then_else: CostingFun {
                mem: ThreeArguments::ConstantCost(
                    *cost_map
                        .get("if_then_else-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: ThreeArguments::ConstantCost(
                    *cost_map
                        .get("if_then_else-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            choose_unit: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("choose_unit-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::ConstantCost(
                    *cost_map
                        .get("choose_unit-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            trace: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map.get("trace-mem-arguments").unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::ConstantCost(
                    *cost_map.get("trace-cpu-arguments").unwrap_or(&30000000000),
                ),
            },
            fst_pair: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("fst_pair-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("fst_pair-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            snd_pair: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("snd_pair-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("snd_pair-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            choose_list: CostingFun {
                mem: ThreeArguments::ConstantCost(
                    *cost_map
                        .get("choose_list-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: ThreeArguments::ConstantCost(
                    *cost_map
                        .get("choose_list-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            mk_cons: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("mk_cons-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::ConstantCost(
                    *cost_map
                        .get("mk_cons-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            head_list: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("head_list-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("head_list-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            tail_list: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("tail_list-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("tail_list-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            null_list: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("null_list-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("null_list-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            choose_data: CostingFun {
                mem: SixArguments::ConstantCost(
                    *cost_map
                        .get("choose_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: SixArguments::ConstantCost(
                    *cost_map
                        .get("choose_data-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            constr_data: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("constr_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::ConstantCost(
                    *cost_map
                        .get("constr_data-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            map_data: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("map_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("map_data-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            list_data: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("list_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("list_data-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            i_data: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map.get("i_data-mem-arguments").unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map.get("i_data-cpu-arguments").unwrap_or(&30000000000),
                ),
            },
            b_data: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map.get("b_data-mem-arguments").unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map.get("b_data-cpu-arguments").unwrap_or(&30000000000),
                ),
            },
            un_constr_data: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("un_constr_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("un_constr_data-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            un_map_data: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("un_map_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("un_map_data-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            un_list_data: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("un_list_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("un_list_data-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            un_i_data: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("un_i_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("un_i_data-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            un_b_data: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("un_b_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("un_b_data-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            equals_data: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("equals_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: *cost_map
                        .get("equals_data-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("equals_data-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            mk_pair_data: CostingFun {
                mem: TwoArguments::ConstantCost(
                    *cost_map
                        .get("mk_pair_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: TwoArguments::ConstantCost(
                    *cost_map
                        .get("mk_pair_data-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            mk_nil_data: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("mk_nil_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("mk_nil_data-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            mk_nil_pair_data: CostingFun {
                mem: OneArgument::ConstantCost(
                    *cost_map
                        .get("mk_nil_pair_data-mem-arguments")
                        .unwrap_or(&30000000000),
                ),
                cpu: OneArgument::ConstantCost(
                    *cost_map
                        .get("mk_nil_pair_data-cpu-arguments")
                        .unwrap_or(&30000000000),
                ),
            },
            serialise_data: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: *cost_map
                        .get("serialise_data-mem-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("serialise_data-mem-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: *cost_map
                        .get("serialise_data-cpu-arguments-intercept")
                        .unwrap_or(&30000000000),
                    slope: *cost_map
                        .get("serialise_data-cpu-arguments-slope")
                        .unwrap_or(&30000000000),
                }),
            },
            blake2b_224: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: OneArgument::ConstantCost(30000000000),
                    mem: OneArgument::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: OneArgument::LinearCost(LinearSize {
                        intercept: *cost_map
                            .get("blake2b_224-cpu-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("blake2b_224-cpu-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                    mem: OneArgument::ConstantCost(
                        *cost_map
                            .get("blake2b_224-mem-arguments-slope")
                            .unwrap_or(&30000000000),
                    ),
                },
            },
            keccak_256: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: OneArgument::ConstantCost(30000000000),
                    mem: OneArgument::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: OneArgument::LinearCost(LinearSize {
                        intercept: *cost_map
                            .get("keccak_256-cpu-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("keccak_256-cpu-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                    mem: OneArgument::ConstantCost(
                        *cost_map
                            .get("keccak_256-mem-arguments")
                            .unwrap_or(&30000000000),
                    ),
                },
            },
            bls12_381_g1_add: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: TwoArguments::ConstantCost(30000000000),
                    mem: TwoArguments::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: TwoArguments::ConstantCost(
                        *cost_map
                            .get("bls12_381_G1_add-cpu-arguments")
                            .unwrap_or(&30000000000),
                    ),
                    mem: TwoArguments::ConstantCost(
                        *cost_map
                            .get("bls12_381_G1_add-mem-arguments")
                            .unwrap_or(&30000000000),
                    ),
                },
            },
            bls12_381_g1_neg: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: OneArgument::ConstantCost(30000000000),
                    mem: OneArgument::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: OneArgument::ConstantCost(
                        *cost_map
                            .get("bls12_381_G1_neg-cpu-arguments")
                            .unwrap_or(&30000000000),
                    ),
                    mem: OneArgument::ConstantCost(
                        *cost_map
                            .get("bls12_381_G1_neg-mem-arguments")
                            .unwrap_or(&30000000000),
                    ),
                },
            },
            bls12_381_g1_scalar_mul: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: TwoArguments::ConstantCost(30000000000),
                    mem: TwoArguments::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: TwoArguments::LinearInX(LinearSize {
                        intercept: *cost_map
                            .get("bls12_381_G1_scalarMul-cpu-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("bls12_381_G1_scalarMul-cpu-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                    mem: TwoArguments::ConstantCost(
                        *cost_map
                            .get("bls12_381_G1_scalarMul-mem-arguments")
                            .unwrap_or(&30000000000),
                    ),
                },
            },
            bls12_381_g1_equal: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: TwoArguments::ConstantCost(30000000000),
                    mem: TwoArguments::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: TwoArguments::ConstantCost(
                        *cost_map
                            .get("bls12_381_G1_equal-cpu-arguments")
                            .unwrap_or(&30000000000),
                    ),
                    mem: TwoArguments::ConstantCost(
                        *cost_map
                            .get("bls12_381_G1_equal-mem-arguments")
                            .unwrap_or(&30000000000),
                    ),
                },
            },
            bls12_381_g1_compress: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: OneArgument::ConstantCost(30000000000),
                    mem: OneArgument::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: OneArgument::ConstantCost(
                        *cost_map
                            .get("bls12_381_G1_compress-cpu-arguments")
                            .unwrap_or(&30000000000),
                    ),
                    mem: OneArgument::ConstantCost(
                        *cost_map
                            .get("bls12_381_G1_compress-mem-arguments")
                            .unwrap_or(&30000000000),
                    ),
                },
            },
            bls12_381_g1_uncompress: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: OneArgument::ConstantCost(30000000000),
                    mem: OneArgument::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: OneArgument::ConstantCost(
                        *cost_map
                            .get("bls12_381_G1_uncompress-cpu-arguments")
                            .unwrap_or(&30000000000),
                    ),
                    mem: OneArgument::ConstantCost(
                        *cost_map
                            .get("bls12_381_G1_uncompress-mem-arguments")
                            .unwrap_or(&30000000000),
                    ),
                },
            },
            bls12_381_g1_hash_to_group: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: TwoArguments::ConstantCost(30000000000),
                    mem: TwoArguments::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: TwoArguments::LinearInX(LinearSize {
                        intercept: *cost_map
                            .get("bls12_381_G1_hashToGroup-cpu-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("bls12_381_G1_hashToGroup-cpu-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                    mem: TwoArguments::ConstantCost(
                        *cost_map
                            .get("bls12_381_G1_hashToGroup-mem-arguments")
                            .unwrap_or(&30000000000),
                    ),
                },
            },
            bls12_381_g2_add: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: TwoArguments::ConstantCost(30000000000),
                    mem: TwoArguments::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: TwoArguments::ConstantCost(
                        *cost_map
                            .get("bls12_381_G2_add-cpu-arguments")
                            .unwrap_or(&30000000000),
                    ),
                    mem: TwoArguments::ConstantCost(
                        *cost_map
                            .get("bls12_381_G2_add-mem-arguments")
                            .unwrap_or(&30000000000),
                    ),
                },
            },
            bls12_381_g2_neg: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: OneArgument::ConstantCost(30000000000),
                    mem: OneArgument::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: OneArgument::ConstantCost(
                        *cost_map
                            .get("bls12_381_G2_neg-cpu-arguments")
                            .unwrap_or(&30000000000),
                    ),
                    mem: OneArgument::ConstantCost(
                        *cost_map
                            .get("bls12_381_G2_neg-mem-arguments")
                            .unwrap_or(&30000000000),
                    ),
                },
            },
            bls12_381_g2_scalar_mul: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: TwoArguments::ConstantCost(30000000000),
                    mem: TwoArguments::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: TwoArguments::LinearInX(LinearSize {
                        intercept: *cost_map
                            .get("bls12_381_G2_scalarMul-cpu-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("bls12_381_G2_scalarMul-cpu-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                    mem: TwoArguments::ConstantCost(
                        *cost_map
                            .get("bls12_381_G2_scalarMul-mem-arguments")
                            .unwrap_or(&30000000000),
                    ),
                },
            },
            bls12_381_g2_equal: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: TwoArguments::ConstantCost(30000000000),
                    mem: TwoArguments::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: TwoArguments::ConstantCost(
                        *cost_map
                            .get("bls12_381_G2_equal-cpu-arguments")
                            .unwrap_or(&30000000000),
                    ),
                    mem: TwoArguments::ConstantCost(
                        *cost_map
                            .get("bls12_381_G2_equal-mem-arguments")
                            .unwrap_or(&30000000000),
                    ),
                },
            },
            bls12_381_g2_compress: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: OneArgument::ConstantCost(30000000000),
                    mem: OneArgument::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: OneArgument::ConstantCost(
                        *cost_map
                            .get("bls12_381_G2_compress-cpu-arguments")
                            .unwrap_or(&30000000000),
                    ),
                    mem: OneArgument::ConstantCost(
                        *cost_map
                            .get("bls12_381_G2_compress-mem-arguments")
                            .unwrap_or(&30000000000),
                    ),
                },
            },
            bls12_381_g2_uncompress: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: OneArgument::ConstantCost(30000000000),
                    mem: OneArgument::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: OneArgument::ConstantCost(
                        *cost_map
                            .get("bls12_381_G2_uncompress-cpu-arguments")
                            .unwrap_or(&30000000000),
                    ),
                    mem: OneArgument::ConstantCost(
                        *cost_map
                            .get("bls12_381_G2_uncompress-mem-arguments")
                            .unwrap_or(&30000000000),
                    ),
                },
            },
            bls12_381_g2_hash_to_group: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: TwoArguments::ConstantCost(30000000000),
                    mem: TwoArguments::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: TwoArguments::LinearInX(LinearSize {
                        intercept: *cost_map
                            .get("bls12_381_G2_hashToGroup-cpu-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("bls12_381_G2_hashToGroup-cpu-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                    mem: TwoArguments::ConstantCost(
                        *cost_map
                            .get("bls12_381_G2_hashToGroup-mem-arguments")
                            .unwrap_or(&30000000000),
                    ),
                },
            },

            bls12_381_miller_loop: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: TwoArguments::ConstantCost(30000000000),
                    mem: TwoArguments::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: TwoArguments::ConstantCost(
                        *cost_map
                            .get("bls12_381_millerLoop-cpu-arguments")
                            .unwrap_or(&30000000000),
                    ),
                    mem: TwoArguments::ConstantCost(
                        *cost_map
                            .get("bls12_381_millerLoop-mem-arguments")
                            .unwrap_or(&30000000000),
                    ),
                },
            },
            bls12_381_mul_ml_result: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: TwoArguments::ConstantCost(30000000000),
                    mem: TwoArguments::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: TwoArguments::ConstantCost(
                        *cost_map
                            .get("bls12_381_mulMlResult-cpu-arguments")
                            .unwrap_or(&30000000000),
                    ),
                    mem: TwoArguments::ConstantCost(
                        *cost_map
                            .get("bls12_381_mulMlResult-mem-arguments")
                            .unwrap_or(&30000000000),
                    ),
                },
            },
            bls12_381_final_verify: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: TwoArguments::ConstantCost(30000000000),
                    mem: TwoArguments::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: TwoArguments::ConstantCost(
                        *cost_map
                            .get("bls12_381_finalVerify-cpu-arguments")
                            .unwrap_or(&30000000000),
                    ),
                    mem: TwoArguments::ConstantCost(
                        *cost_map
                            .get("bls12_381_finalVerify-mem-arguments")
                            .unwrap_or(&30000000000),
                    ),
                },
            },
            integer_to_byte_string: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: ThreeArguments::ConstantCost(30000000000),
                    mem: ThreeArguments::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: ThreeArguments::QuadraticInZ(QuadraticFunction {
                        coeff_0: *cost_map
                            .get("integerToByteString-cpu-arguments-c0")
                            .unwrap_or(&30000000000),
                        coeff_1: *cost_map
                            .get("integerToByteString-cpu-arguments-c1")
                            .unwrap_or(&30000000000),
                        coeff_2: *cost_map
                            .get("integerToByteString-cpu-arguments-c2")
                            .unwrap_or(&30000000000),
                    }),
                    mem: ThreeArguments::LiteralInYorLinearInZ(LinearSize {
                        intercept: *cost_map
                            .get("integerToByteString-mem-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("integerToByteString-mem-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                },
            },
            byte_string_to_integer: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: TwoArguments::ConstantCost(30000000000),
                    mem: TwoArguments::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: TwoArguments::QuadraticInY(QuadraticFunction {
                        coeff_0: *cost_map
                            .get("byteStringToInteger-cpu-arguments-c0")
                            .unwrap_or(&30000000000),
                        coeff_1: *cost_map
                            .get("byteStringToInteger-cpu-arguments-c1")
                            .unwrap_or(&30000000000),
                        coeff_2: *cost_map
                            .get("byteStringToInteger-cpu-arguments-c2")
                            .unwrap_or(&30000000000),
                    }),
                    mem: TwoArguments::LinearInY(LinearSize {
                        intercept: *cost_map
                            .get("byteStringToInteger-mem-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("byteStringToInteger-mem-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                },
            },
            and_byte_string: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: ThreeArguments::ConstantCost(30000000000),
                    mem: ThreeArguments::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: ThreeArguments::LinearInYandZ(TwoVariableLinearSize {
                        intercept: *cost_map
                            .get("andByteString-cpu-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope1: *cost_map
                            .get("andByteString-cpu-arguments-slope1")
                            .unwrap_or(&30000000000),
                        slope2: *cost_map
                            .get("andByteString-cpu-arguments-slope2")
                            .unwrap_or(&30000000000),
                    }),
                    mem: ThreeArguments::LinearInMaxYZ(LinearSize {
                        intercept: *cost_map
                            .get("andByteString-memory-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("andByteString-memory-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                },
            },
            or_byte_string: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: ThreeArguments::ConstantCost(30000000000),
                    mem: ThreeArguments::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: ThreeArguments::LinearInYandZ(TwoVariableLinearSize {
                        intercept: *cost_map
                            .get("orByteString-cpu-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope1: *cost_map
                            .get("orByteString-cpu-arguments-slope1")
                            .unwrap_or(&30000000000),
                        slope2: *cost_map
                            .get("orByteString-cpu-arguments-slope2")
                            .unwrap_or(&30000000000),
                    }),
                    mem: ThreeArguments::LinearInMaxYZ(LinearSize {
                        intercept: *cost_map
                            .get("orByteString-memory-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("orByteString-memory-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                },
            },
            xor_byte_string: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: ThreeArguments::ConstantCost(30000000000),
                    mem: ThreeArguments::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: ThreeArguments::LinearInYandZ(TwoVariableLinearSize {
                        intercept: *cost_map
                            .get("xorByteString-cpu-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope1: *cost_map
                            .get("xorByteString-cpu-arguments-slope1")
                            .unwrap_or(&30000000000),
                        slope2: *cost_map
                            .get("xorByteString-cpu-arguments-slope2")
                            .unwrap_or(&30000000000),
                    }),
                    mem: ThreeArguments::LinearInMaxYZ(LinearSize {
                        intercept: *cost_map
                            .get("xorByteString-memory-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("xorByteString-memory-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                },
            },
            complement_byte_string: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: OneArgument::ConstantCost(30000000000),
                    mem: OneArgument::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: OneArgument::LinearCost(LinearSize {
                        intercept: *cost_map
                            .get("complementByteString-cpu-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("complementByteString-cpu-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                    mem: OneArgument::LinearCost(LinearSize {
                        intercept: *cost_map
                            .get("complementByteString-memory-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("complementByteString-memory-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                },
            },
            read_bit: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: TwoArguments::ConstantCost(30000000000),
                    mem: TwoArguments::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: TwoArguments::ConstantCost(
                        *cost_map
                            .get("readBit-cpu-arguments")
                            .unwrap_or(&30000000000),
                    ),
                    mem: TwoArguments::ConstantCost(
                        *cost_map
                            .get("readBit-memory-arguments")
                            .unwrap_or(&30000000000),
                    ),
                },
            },
            write_bits: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: ThreeArguments::ConstantCost(30000000000),
                    mem: ThreeArguments::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: ThreeArguments::LinearInY(LinearSize {
                        intercept: *cost_map
                            .get("writeBits-cpu-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("writeBits-cpu-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                    mem: ThreeArguments::LinearInX(LinearSize {
                        intercept: *cost_map
                            .get("writeBits-memory-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("writeBits-memory-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                },
            },
            replicate_byte: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: TwoArguments::ConstantCost(30000000000),
                    mem: TwoArguments::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: TwoArguments::LinearInX(LinearSize {
                        intercept: *cost_map
                            .get("replicateByte-cpu-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("replicateByte-cpu-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                    mem: TwoArguments::LinearInX(LinearSize {
                        intercept: *cost_map
                            .get("replicateByte-memory-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("replicateByte-memory-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                },
            },
            shift_byte_string: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: TwoArguments::ConstantCost(30000000000),
                    mem: TwoArguments::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: TwoArguments::LinearInX(LinearSize {
                        intercept: *cost_map
                            .get("shiftByteString-cpu-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("shiftByteString-cpu-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                    mem: TwoArguments::LinearInX(LinearSize {
                        intercept: *cost_map
                            .get("shiftByteString-memory-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("shiftByteString-memory-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                },
            },
            rotate_byte_string: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: TwoArguments::ConstantCost(30000000000),
                    mem: TwoArguments::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: TwoArguments::LinearInX(LinearSize {
                        intercept: *cost_map
                            .get("rotateByteString-cpu-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("rotateByteString-cpu-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                    mem: TwoArguments::LinearInX(LinearSize {
                        intercept: *cost_map
                            .get("rotateByteString-memory-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("rotateByteString-memory-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                },
            },
            count_set_bits: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: OneArgument::ConstantCost(30000000000),
                    mem: OneArgument::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: OneArgument::LinearCost(LinearSize {
                        intercept: *cost_map
                            .get("countSetBits-cpu-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("countSetBits-cpu-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                    mem: OneArgument::ConstantCost(
                        *cost_map
                            .get("countSetBits-memory-arguments")
                            .unwrap_or(&30000000000),
                    ),
                },
            },
            find_first_set_bit: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: OneArgument::ConstantCost(30000000000),
                    mem: OneArgument::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: OneArgument::LinearCost(LinearSize {
                        intercept: *cost_map
                            .get("findFirstSetBit-cpu-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("findFirstSetBit-cpu-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                    mem: OneArgument::ConstantCost(
                        *cost_map
                            .get("findFirstSetBit-memory-arguments")
                            .unwrap_or(&30000000000),
                    ),
                },
            },
            ripemd_160: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: OneArgument::ConstantCost(30000000000),
                    mem: OneArgument::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: OneArgument::LinearCost(LinearSize {
                        intercept: *cost_map
                            .get("ripemd_160-cpu-arguments-intercept")
                            .unwrap_or(&30000000000),
                        slope: *cost_map
                            .get("ripemd_160-cpu-arguments-slope")
                            .unwrap_or(&30000000000),
                    }),
                    mem: OneArgument::ConstantCost(
                        *cost_map
                            .get("ripemd_160-memory-arguments")
                            .unwrap_or(&30000000000),
                    ),
                },
            },
            exp_mod_int: match version {
                Language::PlutusV1 | Language::PlutusV2 => CostingFun {
                    cpu: ThreeArguments::ConstantCost(30000000000),
                    mem: ThreeArguments::ConstantCost(30000000000),
                },
                Language::PlutusV3 => CostingFun {
                    cpu: ThreeArguments::ConstantCost(
                        *cost_map
                            .get("expModInteger-cpu-arguments")
                            .unwrap_or(&30000000000),
                    ),
                    mem: ThreeArguments::ConstantCost(
                        *cost_map
                            .get("expModInteger-memory-arguments")
                            .unwrap_or(&30000000000),
                    ),
                },
            },
        },
    }
}

#[derive(Debug, PartialEq)]
pub struct CostingFun<T> {
    pub mem: T,
    pub cpu: T,
}

#[derive(Debug, PartialEq)]
pub enum OneArgument {
    ConstantCost(i64),
    LinearCost(LinearSize),
}

impl OneArgument {
    pub fn cost(&self, x: i64) -> i64 {
        match self {
            OneArgument::ConstantCost(c) => *c,
            OneArgument::LinearCost(m) => m.slope * x + m.intercept,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TwoArguments {
    ConstantCost(i64),
    LinearInX(LinearSize),
    LinearInY(LinearSize),
    LinearInXAndY(TwoVariableLinearSize),
    AddedSizes(AddedSizes),
    SubtractedSizes(SubtractedSizes),
    MultipliedSizes(MultipliedSizes),
    MinSize(MinSize),
    MaxSize(MaxSize),
    LinearOnDiagonal(ConstantOrLinear),
    ConstAboveDiagonal(ConstantOrTwoArguments),
    ConstBelowDiagonal(ConstantOrTwoArguments),
    QuadraticInY(QuadraticFunction),
    ConstAboveDiagonalIntoQuadraticXAndY(i64, TwoArgumentsQuadraticFunction),
}

impl TwoArguments {
    pub fn cost(&self, x: i64, y: i64) -> i64 {
        match self {
            TwoArguments::ConstantCost(c) => *c,
            TwoArguments::LinearInX(l) => l.slope * x + l.intercept,
            TwoArguments::LinearInY(l) => l.slope * y + l.intercept,
            TwoArguments::LinearInXAndY(l) => l.slope1 * x + l.slope2 * y + l.intercept,
            TwoArguments::AddedSizes(s) => s.slope * (x + y) + s.intercept,
            TwoArguments::SubtractedSizes(s) => s.slope * s.minimum.max(x - y) + s.intercept,
            TwoArguments::MultipliedSizes(s) => s.slope * (x * y) + s.intercept,
            TwoArguments::MinSize(s) => s.slope * x.min(y) + s.intercept,
            TwoArguments::MaxSize(s) => s.slope * x.max(y) + s.intercept,
            TwoArguments::LinearOnDiagonal(l) => {
                if x == y {
                    x * l.slope + l.intercept
                } else {
                    l.constant
                }
            }
            TwoArguments::ConstAboveDiagonal(l) => {
                if x < y {
                    l.constant
                } else {
                    let p = *l.model.clone();
                    p.cost(x, y)
                }
            }
            TwoArguments::ConstBelowDiagonal(l) => {
                if x > y {
                    l.constant
                } else {
                    let p = *l.model.clone();
                    p.cost(x, y)
                }
            }
            TwoArguments::QuadraticInY(q) => q.coeff_0 + (q.coeff_1 * y) + (q.coeff_2 * y * y),
            TwoArguments::ConstAboveDiagonalIntoQuadraticXAndY(constant, q) => {
                if x < y {
                    *constant
                } else {
                    std::cmp::max(
                        q.minimum,
                        q.coeff_00
                            + q.coeff_10 * x
                            + q.coeff_01 * y
                            + q.coeff_20 * x * x
                            + q.coeff_11 * x * y
                            + q.coeff_02 * y * y,
                    )
                }
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ThreeArguments {
    ConstantCost(i64),
    AddedSizes(AddedSizes),
    LinearInX(LinearSize),
    LinearInY(LinearSize),
    LinearInZ(LinearSize),
    QuadraticInZ(QuadraticFunction),
    LiteralInYorLinearInZ(LinearSize),
    LinearInMaxYZ(LinearSize),
    LinearInYandZ(TwoVariableLinearSize),
}

impl ThreeArguments {
    pub fn cost(&self, x: i64, y: i64, z: i64) -> i64 {
        match self {
            ThreeArguments::ConstantCost(c) => *c,
            ThreeArguments::AddedSizes(s) => (x + y + z) * s.slope + s.intercept,
            ThreeArguments::LinearInX(l) => x * l.slope + l.intercept,
            ThreeArguments::LinearInY(l) => y * l.slope + l.intercept,
            ThreeArguments::LinearInZ(l) => z * l.slope + l.intercept,
            ThreeArguments::QuadraticInZ(q) => q.coeff_0 + (q.coeff_1 * z) + (q.coeff_2 * z * z),
            ThreeArguments::LiteralInYorLinearInZ(l) => {
                if y == 0 {
                    l.slope * z + l.intercept
                } else {
                    y
                }
            }
            ThreeArguments::LinearInMaxYZ(l) => y.max(z) * l.slope + l.intercept,
            ThreeArguments::LinearInYandZ(l) => y * l.slope1 + z * l.slope2 + l.intercept,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum SixArguments {
    ConstantCost(i64),
}

impl SixArguments {
    pub fn cost(&self, _: i64, _: i64, _: i64, _: i64, _: i64, _: i64) -> i64 {
        match self {
            SixArguments::ConstantCost(c) => *c,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LinearSize {
    pub intercept: i64,
    pub slope: i64,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TwoVariableLinearSize {
    pub intercept: i64,
    pub slope1: i64,
    pub slope2: i64,
}

#[derive(Debug, PartialEq, Clone)]
pub struct AddedSizes {
    pub intercept: i64,
    pub slope: i64,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SubtractedSizes {
    pub intercept: i64,
    pub slope: i64,
    pub minimum: i64,
}

#[derive(Debug, PartialEq, Clone)]
pub struct MultipliedSizes {
    pub intercept: i64,
    pub slope: i64,
}

#[derive(Debug, PartialEq, Clone)]
pub struct MinSize {
    pub intercept: i64,
    pub slope: i64,
}

#[derive(Debug, PartialEq, Clone)]
pub struct MaxSize {
    pub intercept: i64,
    pub slope: i64,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ConstantOrLinear {
    pub constant: i64,
    pub intercept: i64,
    pub slope: i64,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ConstantOrTwoArguments {
    pub constant: i64,
    pub model: Box<TwoArguments>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct QuadraticFunction {
    coeff_0: i64,
    coeff_1: i64,
    coeff_2: i64,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TwoArgumentsQuadraticFunction {
    minimum: i64,
    coeff_00: i64,
    coeff_10: i64,
    coeff_01: i64,
    coeff_20: i64,
    coeff_11: i64,
    coeff_02: i64,
}

#[repr(u8)]
pub enum StepKind {
    Constant = 0,
    Var = 1,
    Lambda = 2,
    Apply = 3,
    Delay = 4,
    Force = 5,
    Builtin = 6,
    Constr = 7,
    Case = 8,
    // DO NOT USE THIS IN `step_and_maybe_spend`
    StartUp = 9,
}

impl TryFrom<u8> for StepKind {
    type Error = super::error::Error;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(StepKind::Constant),
            1 => Ok(StepKind::Var),
            2 => Ok(StepKind::Lambda),
            3 => Ok(StepKind::Apply),
            4 => Ok(StepKind::Delay),
            5 => Ok(StepKind::Force),
            6 => Ok(StepKind::Builtin),
            7 => Ok(StepKind::Constr),
            8 => Ok(StepKind::Case),
            v => Err(super::error::Error::InvalidStepKind(v)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn assert_default_cost_model_v1_mainnet_2024_09_29() {
        let costs = vec![
            100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356, 4,
            16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100,
            16000, 100, 94375, 32, 132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189, 769,
            4, 2, 85848, 228465, 122, 0, 1, 1, 1000, 42921, 4, 2, 24548, 29498, 38, 1, 898148,
            27279, 1, 51775, 558, 1, 39184, 1000, 60594, 1, 141895, 32, 83150, 32, 15299, 32,
            76049, 1, 13169, 4, 22100, 10, 28999, 74, 1, 28999, 74, 1, 43285, 552, 1, 44749, 541,
            1, 33852, 32, 68246, 32, 72362, 32, 7243, 32, 7391, 32, 11546, 32, 85848, 228465, 122,
            0, 1, 1, 90434, 519, 0, 1, 74433, 32, 85848, 228465, 122, 0, 1, 1, 85848, 228465, 122,
            0, 1, 1, 270652, 22588, 4, 1457325, 64566, 4, 20467, 1, 4, 0, 141992, 32, 100788, 420,
            1, 1, 81663, 32, 59498, 32, 20142, 32, 24588, 32, 20744, 32, 25933, 32, 24623, 32,
            53384111, 14333, 10,
        ];

        let cost_model = initialize_cost_model(&Language::PlutusV1, &costs);

        assert_eq!(CostModel::v1(), cost_model);
    }

    #[test]
    fn assert_default_cost_model_v2_mainnet_2024_09_29() {
        let costs = vec![
            100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356, 4,
            16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100,
            16000, 100, 94375, 32, 132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189, 769,
            4, 2, 85848, 228465, 122, 0, 1, 1, 1000, 42921, 4, 2, 24548, 29498, 38, 1, 898148,
            27279, 1, 51775, 558, 1, 39184, 1000, 60594, 1, 141895, 32, 83150, 32, 15299, 32,
            76049, 1, 13169, 4, 22100, 10, 28999, 74, 1, 28999, 74, 1, 43285, 552, 1, 44749, 541,
            1, 33852, 32, 68246, 32, 72362, 32, 7243, 32, 7391, 32, 11546, 32, 85848, 228465, 122,
            0, 1, 1, 90434, 519, 0, 1, 74433, 32, 85848, 228465, 122, 0, 1, 1, 85848, 228465, 122,
            0, 1, 1, 955506, 213312, 0, 2, 270652, 22588, 4, 1457325, 64566, 4, 20467, 1, 4, 0,
            141992, 32, 100788, 420, 1, 1, 81663, 32, 59498, 32, 20142, 32, 24588, 32, 20744, 32,
            25933, 32, 24623, 32, 43053543, 10, 53384111, 14333, 10, 43574283, 26308, 10,
        ];

        let cost_model = initialize_cost_model(&Language::PlutusV2, &costs);

        assert_eq!(CostModel::v2(), cost_model);
    }

    // #[test]
    // fn assert_default_cost_model_v3_mainnet_2024_11_30() {
    //     let costs: Vec<i64> = vec![
    //         100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356, 4,
    //         16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100,
    //         16000, 100, 94375, 32, 132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189, 769,
    //         4, 2, 85848, 123203, 7305, -900, 1716, 549, 57, 85848, 0, 1, 1, 1000, 42921, 4, 2,
    //         24548, 29498, 38, 1, 898148, 27279, 1, 51775, 558, 1, 39184, 1000, 60594, 1, 141895,
    //         32, 83150, 32, 15299, 32, 76049, 1, 13169, 4, 22100, 10, 28999, 74, 1, 28999, 74, 1,
    //         43285, 552, 1, 44749, 541, 1, 33852, 32, 68246, 32, 72362, 32, 7243, 32, 7391, 32,
    //         11546, 32, 85848, 123203, 7305, -900, 1716, 549, 57, 85848, 0, 1, 90434, 519, 0, 1,
    //         74433, 32, 85848, 123203, 7305, -900, 1716, 549, 57, 85848, 0, 1, 1, 85848, 123203,
    //         7305, -900, 1716, 549, 57, 85848, 0, 1, 955506, 213312, 0, 2, 270652, 22588, 4,
    //         1457325, 64566, 4, 20467, 1, 4, 0, 141992, 32, 100788, 420, 1, 1, 81663, 32, 59498, 32,
    //         20142, 32, 24588, 32, 20744, 32, 25933, 32, 24623, 32, 43053543, 10, 53384111, 14333,
    //         10, 43574283, 26308, 10, 16000, 100, 16000, 100, 962335, 18, 2780678, 6, 442008, 1,
    //         52538055, 3756, 18, 267929, 18, 76433006, 8868, 18, 52948122, 18, 1995836, 36, 3227919,
    //         12, 901022, 1, 166917843, 4307, 36, 284546, 36, 158221314, 26549, 36, 74698472, 36,
    //         333849714, 1, 254006273, 72, 2174038, 72, 2261318, 64571, 4, 207616, 8310, 4, 1293828,
    //         28716, 63, 0, 1, 1006041, 43623, 251, 0, 1,
    //     ];

    //     let cost_model = initialize_cost_model(&Language::PlutusV3, &costs);

    //     assert_eq!(CostModel::v3(), cost_model);
    // }

    #[test]
    fn assert_default_cost_model_v3_preprod_2024_11_22() {
        let costs: Vec<i64> = vec![
            100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356, 4,
            16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100,
            16000, 100, 94375, 32, 132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189, 769,
            4, 2, 85848, 123203, 7305, -900, 1716, 549, 57, 85848, 0, 1, 1, 1000, 42921, 4, 2,
            24548, 29498, 38, 1, 898148, 27279, 1, 51775, 558, 1, 39184, 1000, 60594, 1, 141895,
            32, 83150, 32, 15299, 32, 76049, 1, 13169, 4, 22100, 10, 28999, 74, 1, 28999, 74, 1,
            43285, 552, 1, 44749, 541, 1, 33852, 32, 68246, 32, 72362, 32, 7243, 32, 7391, 32,
            11546, 32, 85848, 123203, 7305, -900, 1716, 549, 57, 85848, 0, 1, 90434, 519, 0, 1,
            74433, 32, 85848, 123203, 7305, -900, 1716, 549, 57, 85848, 0, 1, 1, 85848, 123203,
            7305, -900, 1716, 549, 57, 85848, 0, 1, 955506, 213312, 0, 2, 270652, 22588, 4,
            1457325, 64566, 4, 20467, 1, 4, 0, 141992, 32, 100788, 420, 1, 1, 81663, 32, 59498, 32,
            20142, 32, 24588, 32, 20744, 32, 25933, 32, 24623, 32, 43053543, 10, 53384111, 14333,
            10, 43574283, 26308, 10, 16000, 100, 16000, 100, 962335, 18, 2780678, 6, 442008, 1,
            52538055, 3756, 18, 267929, 18, 76433006, 8868, 18, 52948122, 18, 1995836, 36, 3227919,
            12, 901022, 1, 166917843, 4307, 36, 284546, 36, 158221314, 26549, 36, 74698472, 36,
            333849714, 1, 254006273, 72, 2174038, 72, 2261318, 64571, 4, 207616, 8310, 4, 1293828,
            28716, 63, 0, 1, 1006041, 43623, 251, 0, 1, 100181, 726, 719, 0, 1, 100181, 726, 719,
            0, 1, 100181, 726, 719, 0, 1, 107878, 680, 0, 1, 95336, 1, 281145, 18848, 0, 1, 180194,
            159, 1, 1, 158519, 8942, 0, 1, 159378, 8813, 0, 1, 107490, 3298, 1, 106057, 655, 1,
            1964219, 24520, 3,
        ];

        let cost_model = initialize_cost_model(&Language::PlutusV3, &costs);

        assert_eq!(CostModel::v3(), cost_model);
    }
}
