use crate::builtins::DefaultFunction;

use super::Value;

/// Can be negative
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct ExBudget {
    pub mem: i64,
    pub cpu: i64,
}

impl ExBudget {
    pub fn occurences(&mut self, n: i64) {
        self.mem *= n;
        self.cpu *= n;
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

#[derive(Default)]
pub struct CostModel {
    pub machine_costs: MachineCosts,
    pub builtin_costs: BuiltinCosts,
}

/// There's no entry for Error since we'll be exiting anyway; also, what would
/// happen if calling 'Error' caused the budget to be exceeded?
pub struct MachineCosts {
    startup: ExBudget,
    var: ExBudget,
    constant: ExBudget,
    lambda: ExBudget,
    delay: ExBudget,
    force: ExBudget,
    apply: ExBudget,
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
            StepKind::StartUp => self.startup,
        }
    }
}

impl Default for MachineCosts {
    fn default() -> Self {
        Self {
            startup: ExBudget { mem: 100, cpu: 100 },
            var: ExBudget {
                mem: 100,
                cpu: 23000,
            },
            constant: ExBudget {
                mem: 100,
                cpu: 23000,
            },
            lambda: ExBudget {
                mem: 100,
                cpu: 23000,
            },
            delay: ExBudget {
                mem: 100,
                cpu: 23000,
            },
            force: ExBudget {
                mem: 100,
                cpu: 23000,
            },
            apply: ExBudget {
                mem: 100,
                cpu: 23000,
            },
            builtin: ExBudget {
                mem: 100,
                cpu: 23000,
            },
        }
    }
}

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
    pub blake2b_256: CostingFun<OneArgument>,
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
}

impl Default for BuiltinCosts {
    fn default() -> Self {
        Self {
            add_integer: CostingFun {
                mem: TwoArguments::MaxSize(MaxSize {
                    intercept: 1,
                    slope: 1,
                }),
                cpu: TwoArguments::MaxSize(MaxSize {
                    intercept: 205665,
                    slope: 812,
                }),
            },
            subtract_integer: CostingFun {
                mem: TwoArguments::MaxSize(MaxSize {
                    intercept: 1,
                    slope: 1,
                }),
                cpu: TwoArguments::MaxSize(MaxSize {
                    intercept: 205665,
                    slope: 812,
                }),
            },
            multiply_integer: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 0,
                    slope: 1,
                }),

                cpu: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 69522,
                    slope: 11687,
                }),
            },
            divide_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: 0,
                    slope: 1,
                    minimum: 1,
                }),
                cpu: TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                    constant: 196500,
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: 453240,
                        slope: 220,
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
                    constant: 196500,
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: 453240,
                        slope: 220,
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
                    constant: 196500,
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: 453240,
                        slope: 220,
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
                    constant: 196500,
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: 453240,
                        slope: 220,
                    })),
                }),
            },
            equals_integer: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 208512,
                    slope: 421,
                }),
            },
            less_than_integer: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 208896,
                    slope: 511,
                }),
            },
            less_than_equals_integer: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 204924,
                    slope: 473,
                }),
            },
            append_byte_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 1000,
                    slope: 571,
                }),
                cpu: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 0,
                    slope: 1,
                }),
            },
            cons_byte_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 0,
                    slope: 1,
                }),
                cpu: TwoArguments::LinearInY(LinearSize {
                    intercept: 221973,
                    slope: 511,
                }),
            },
            slice_byte_string: CostingFun {
                mem: ThreeArguments::LinearInZ(LinearSize {
                    intercept: 4,
                    slope: 0,
                }),
                cpu: ThreeArguments::LinearInZ(LinearSize {
                    intercept: 265318,
                    slope: 0,
                }),
            },
            length_of_byte_string: CostingFun {
                mem: OneArgument::ConstantCost(10),
                cpu: OneArgument::ConstantCost(1000),
            },
            index_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(4),
                cpu: TwoArguments::ConstantCost(57667),
            },
            equals_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::LinearOnDiagonal(ConstantOrLinear {
                    constant: 245000,
                    intercept: 216773,
                    slope: 62,
                }),
            },
            less_than_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 197145,
                    slope: 156,
                }),
            },
            less_than_equals_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 197145,
                    slope: 156,
                }),
            },
            sha2_256: CostingFun {
                mem: OneArgument::ConstantCost(4),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 806990,
                    slope: 30482,
                }),
            },
            sha3_256: CostingFun {
                mem: OneArgument::ConstantCost(4),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 1927926,
                    slope: 82523,
                }),
            },
            blake2b_256: CostingFun {
                mem: OneArgument::ConstantCost(4),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 117366,
                    slope: 10475,
                }),
            },
            verify_ed25519_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(10),
                cpu: ThreeArguments::LinearInZ(LinearSize {
                    intercept: 41047009,
                    slope: 18816,
                }),
            },
            verify_ecdsa_secp256k1_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(10),
                cpu: ThreeArguments::ConstantCost(35190005),
            },
            verify_schnorr_secp256k1_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(10),
                cpu: ThreeArguments::LinearInY(LinearSize {
                    intercept: 39121781,
                    slope: 32260,
                }),
            },
            append_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 4,
                    slope: 1,
                }),
                cpu: TwoArguments::AddedSizes(AddedSizes {
                    intercept: 1000,
                    slope: 24177,
                }),
            },
            equals_string: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::LinearOnDiagonal(ConstantOrLinear {
                    constant: 187000,
                    intercept: 1000,
                    slope: 52998,
                }),
            },
            encode_utf8: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: 4,
                    slope: 2,
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 1000,
                    slope: 28662,
                }),
            },
            decode_utf8: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: 4,
                    slope: 2,
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 497525,
                    slope: 14068,
                }),
            },
            if_then_else: CostingFun {
                mem: ThreeArguments::ConstantCost(1),
                cpu: ThreeArguments::ConstantCost(80556),
            },
            choose_unit: CostingFun {
                mem: TwoArguments::ConstantCost(4),
                cpu: TwoArguments::ConstantCost(46417),
            },
            trace: CostingFun {
                mem: TwoArguments::ConstantCost(32),
                cpu: TwoArguments::ConstantCost(212342),
            },
            fst_pair: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(80436),
            },
            snd_pair: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(85931),
            },
            choose_list: CostingFun {
                mem: ThreeArguments::ConstantCost(32),
                cpu: ThreeArguments::ConstantCost(175354),
            },
            mk_cons: CostingFun {
                mem: TwoArguments::ConstantCost(32),
                cpu: TwoArguments::ConstantCost(65493),
            },
            head_list: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(43249),
            },
            tail_list: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(41182),
            },
            null_list: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(60091),
            },
            choose_data: CostingFun {
                mem: SixArguments::ConstantCost(32),
                cpu: SixArguments::ConstantCost(19537),
            },
            constr_data: CostingFun {
                mem: TwoArguments::ConstantCost(32),
                cpu: TwoArguments::ConstantCost(89141),
            },
            map_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(64832),
            },
            list_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(52467),
            },
            i_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(1000),
            },
            b_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(1000),
            },
            un_constr_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(32696),
            },
            un_map_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(38314),
            },
            un_list_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(32247),
            },
            un_i_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(43357),
            },
            un_b_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(31220),
            },
            equals_data: CostingFun {
                mem: TwoArguments::ConstantCost(1),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: 1060367,
                    slope: 12586,
                }),
            },
            mk_pair_data: CostingFun {
                mem: TwoArguments::ConstantCost(32),
                cpu: TwoArguments::ConstantCost(76511),
            },
            mk_nil_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(22558),
            },
            mk_nil_pair_data: CostingFun {
                mem: OneArgument::ConstantCost(32),
                cpu: OneArgument::ConstantCost(16563),
            },
            serialise_data: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: 0,
                    slope: 2,
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: 1159724,
                    slope: 392670,
                }),
            },
        }
    }
}

impl BuiltinCosts {
    pub fn to_ex_budget(&self, fun: DefaultFunction, args: &[Value]) -> ExBudget {
        match fun {
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
            DefaultFunction::QuotientInteger => todo!(),
            DefaultFunction::RemainderInteger => todo!(),
            DefaultFunction::ModInteger => todo!(),
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
            DefaultFunction::VerifySignature => todo!(),
            DefaultFunction::VerifyEcdsaSecp256k1Signature => todo!(),
            DefaultFunction::VerifySchnorrSecp256k1Signature => todo!(),
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
            DefaultFunction::FstPair => todo!(),
            DefaultFunction::SndPair => todo!(),
            DefaultFunction::ChooseList => todo!(),
            DefaultFunction::MkCons => todo!(),
            DefaultFunction::HeadList => todo!(),
            DefaultFunction::TailList => todo!(),
            DefaultFunction::NullList => todo!(),
            DefaultFunction::ChooseData => todo!(),
            DefaultFunction::ConstrData => todo!(),
            DefaultFunction::MapData => todo!(),
            DefaultFunction::ListData => todo!(),
            DefaultFunction::IData => todo!(),
            DefaultFunction::BData => todo!(),
            DefaultFunction::UnConstrData => todo!(),
            DefaultFunction::UnMapData => todo!(),
            DefaultFunction::UnListData => todo!(),
            DefaultFunction::UnIData => todo!(),
            DefaultFunction::UnBData => todo!(),
            DefaultFunction::EqualsData => todo!(),
            DefaultFunction::SerialiseData => todo!(),
            DefaultFunction::MkPairData => todo!(),
            DefaultFunction::MkNilData => todo!(),
            DefaultFunction::MkNilPairData => todo!(),
        }
    }
}

pub struct CostingFun<T> {
    pub mem: T,
    pub cpu: T,
}

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
#[derive(Clone)]
pub enum TwoArguments {
    ConstantCost(i64),
    LinearInX(LinearSize),
    LinearInY(LinearSize),
    AddedSizes(AddedSizes),
    SubtractedSizes(SubtractedSizes),
    MultipliedSizes(MultipliedSizes),
    MinSize(MinSize),
    MaxSize(MaxSize),
    LinearOnDiagonal(ConstantOrLinear),
    ConstAboveDiagonal(ConstantOrTwoArguments),
    ConstBelowDiagonal(ConstantOrTwoArguments),
}
impl TwoArguments {
    pub fn cost(&self, x: i64, y: i64) -> i64 {
        match self {
            TwoArguments::ConstantCost(c) => *c,
            TwoArguments::LinearInX(l) => l.slope * x + l.intercept,
            TwoArguments::LinearInY(l) => l.slope * y + l.intercept,
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
                if x > y {
                    l.constant
                } else {
                    let p = *l.model.clone();
                    p.cost(x, y)
                }
            }
            TwoArguments::ConstBelowDiagonal(l) => {
                if x < y {
                    l.constant
                } else {
                    let p = *l.model.clone();
                    p.cost(x, y)
                }
            }
        }
    }
}

pub enum ThreeArguments {
    ConstantCost(i64),
    AddedSizes(AddedSizes),
    LinearInX(LinearSize),
    LinearInY(LinearSize),
    LinearInZ(LinearSize),
}

impl ThreeArguments {
    pub fn cost(&self, x: i64, y: i64, z: i64) -> i64 {
        match self {
            ThreeArguments::ConstantCost(c) => *c,
            ThreeArguments::AddedSizes(s) => (x + y + z) * s.slope + s.intercept,
            ThreeArguments::LinearInX(l) => x * l.slope + l.intercept,
            ThreeArguments::LinearInY(l) => y * l.slope + l.intercept,
            ThreeArguments::LinearInZ(l) => z * l.slope + l.intercept,
        }
    }
}

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

#[derive(Clone)]
pub struct LinearSize {
    pub intercept: i64,
    pub slope: i64,
}

#[derive(Clone)]
pub struct AddedSizes {
    pub intercept: i64,
    pub slope: i64,
}

#[derive(Clone)]
pub struct SubtractedSizes {
    pub intercept: i64,
    pub slope: i64,
    pub minimum: i64,
}

#[derive(Clone)]
pub struct MultipliedSizes {
    pub intercept: i64,
    pub slope: i64,
}

#[derive(Clone)]
pub struct MinSize {
    pub intercept: i64,
    pub slope: i64,
}

#[derive(Clone)]
pub struct MaxSize {
    pub intercept: i64,
    pub slope: i64,
}

#[derive(Clone)]
pub struct ConstantOrLinear {
    pub constant: i64,
    pub intercept: i64,
    pub slope: i64,
}

#[derive(Clone)]
pub struct ConstantOrTwoArguments {
    pub constant: i64,
    pub model: Box<TwoArguments>,
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
    // DO NOT USE THIS IN `step_and_maybe_spend`
    StartUp = 7,
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
            v => Err(super::error::Error::InvalidStepKind(v)),
        }
    }
}
