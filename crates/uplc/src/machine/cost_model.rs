use super::value::integer_log2;
use super::{Error, Value, runtime::BuiltinSemantics};
use crate::builtins::DefaultFunction;
use num_bigint::BigInt;
use num_traits::Signed;
use pallas_primitives::conway::Language;
use std::collections::HashMap;
use strum::{Display, EnumIter};

// Existing default cost models use a large sentinel for builtins that are not
// available in a language version.
const UNAVAILABLE_BUILTIN_COST_PLACEHOLDER: i64 = 30000000000;

/// Can be negative
#[derive(Debug, Clone, PartialEq, Eq, Copy, serde::Serialize)]
pub struct ExBudget {
    pub mem: i64,
    pub cpu: i64,
}

impl ExBudget {
    pub fn default_startup_cost() -> Self {
        Self { mem: 100, cpu: 100 }
    }

    pub fn default_machine_cost() -> Self {
        Self {
            mem: 100,
            cpu: 16000,
        }
    }

    pub fn unavailable_machine_cost() -> Self {
        Self {
            mem: UNAVAILABLE_BUILTIN_COST_PLACEHOLDER,
            cpu: UNAVAILABLE_BUILTIN_COST_PLACEHOLDER,
        }
    }

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
            mem: 16500000,
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

    /// Local hard-coded default cost model with builtin semantics selected for
    /// the requested protocol version. This is not ledger-supplied protocol
    /// params; callers with explicit params should use
    /// `initialize_cost_model_with_protocol` instead.
    pub fn default_for_language_and_protocol(
        version: &Language,
        protocol_major_version: u16,
    ) -> Self {
        let costs = match version {
            Language::PlutusV1 => &BuiltinCosts::DEFAULT_V1[..],
            Language::PlutusV2 => &BuiltinCosts::DEFAULT_V2[..],
            Language::PlutusV3 => &BuiltinCosts::DEFAULT_V3[..],
        };

        initialize_cost_model_with_protocol(version, protocol_major_version, costs)
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
            startup: ExBudget::default_startup_cost(),
            var: ExBudget::default_machine_cost(),
            constant: ExBudget::default_machine_cost(),
            lambda: ExBudget::default_machine_cost(),
            delay: ExBudget::default_machine_cost(),
            force: ExBudget::default_machine_cost(),
            apply: ExBudget::default_machine_cost(),
            builtin: ExBudget::default_machine_cost(),
            constr: ExBudget::unavailable_machine_cost(),
            case: ExBudget::unavailable_machine_cost(),
        }
    }

    pub fn v2() -> Self {
        Self {
            startup: ExBudget::default_startup_cost(),
            var: ExBudget::default_machine_cost(),
            constant: ExBudget::default_machine_cost(),
            lambda: ExBudget::default_machine_cost(),
            delay: ExBudget::default_machine_cost(),
            force: ExBudget::default_machine_cost(),
            apply: ExBudget::default_machine_cost(),
            builtin: ExBudget::default_machine_cost(),
            constr: ExBudget::unavailable_machine_cost(),
            case: ExBudget::unavailable_machine_cost(),
        }
    }

    pub fn v3() -> Self {
        Self {
            startup: ExBudget::default_startup_cost(),
            var: ExBudget::default_machine_cost(),
            constant: ExBudget::default_machine_cost(),
            lambda: ExBudget::default_machine_cost(),
            delay: ExBudget::default_machine_cost(),
            force: ExBudget::default_machine_cost(),
            apply: ExBudget::default_machine_cost(),
            builtin: ExBudget::default_machine_cost(),
            constr: ExBudget::default_machine_cost(),
            case: ExBudget::default_machine_cost(),
        }
    }
}

impl Default for MachineCosts {
    fn default() -> Self {
        MachineCosts::v3()
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum ParamName {
    AddInteger_cpu_arguments_intercept,
    AddInteger_cpu_arguments_slope,
    AddInteger_memory_arguments_intercept,
    AddInteger_memory_arguments_slope,
    AndByteString_cpu_arguments_intercept,
    AndByteString_cpu_arguments_slope1,
    AndByteString_cpu_arguments_slope2,
    AndByteString_memory_arguments_intercept,
    AndByteString_memory_arguments_slope,
    AppendByteString_cpu_arguments_intercept,
    AppendByteString_cpu_arguments_slope,
    AppendByteString_memory_arguments_intercept,
    AppendByteString_memory_arguments_slope,
    AppendString_cpu_arguments_intercept,
    AppendString_cpu_arguments_slope,
    AppendString_memory_arguments_intercept,
    AppendString_memory_arguments_slope,
    BData_cpu_arguments,
    BData_memory_arguments,
    Blake2b_224_cpu_arguments_intercept,
    Blake2b_224_cpu_arguments_slope,
    Blake2b_224_memory_arguments,
    Blake2b_256_cpu_arguments_intercept,
    Blake2b_256_cpu_arguments_slope,
    Blake2b_256_memory_arguments,
    Bls12_381_G1_add_cpu_arguments,
    Bls12_381_G1_add_memory_arguments,
    Bls12_381_G1_compress_cpu_arguments,
    Bls12_381_G1_compress_memory_arguments,
    Bls12_381_G1_equal_cpu_arguments,
    Bls12_381_G1_equal_memory_arguments,
    Bls12_381_G1_hashToGroup_cpu_arguments_intercept,
    Bls12_381_G1_hashToGroup_cpu_arguments_slope,
    Bls12_381_G1_hashToGroup_memory_arguments,
    Bls12_381_G1_multiScalarMul_cpu_arguments_intercept,
    Bls12_381_G1_multiScalarMul_cpu_arguments_slope,
    Bls12_381_G1_multiScalarMul_memory_arguments,
    Bls12_381_G1_neg_cpu_arguments,
    Bls12_381_G1_neg_memory_arguments,
    Bls12_381_G1_scalarMul_cpu_arguments_intercept,
    Bls12_381_G1_scalarMul_cpu_arguments_slope,
    Bls12_381_G1_scalarMul_memory_arguments,
    Bls12_381_G1_uncompress_cpu_arguments,
    Bls12_381_G1_uncompress_memory_arguments,
    Bls12_381_G2_add_cpu_arguments,
    Bls12_381_G2_add_memory_arguments,
    Bls12_381_G2_compress_cpu_arguments,
    Bls12_381_G2_compress_memory_arguments,
    Bls12_381_G2_equal_cpu_arguments,
    Bls12_381_G2_equal_memory_arguments,
    Bls12_381_G2_hashToGroup_cpu_arguments_intercept,
    Bls12_381_G2_hashToGroup_cpu_arguments_slope,
    Bls12_381_G2_hashToGroup_memory_arguments,
    Bls12_381_G2_multiScalarMul_cpu_arguments_intercept,
    Bls12_381_G2_multiScalarMul_cpu_arguments_slope,
    Bls12_381_G2_multiScalarMul_memory_arguments,
    Bls12_381_G2_neg_cpu_arguments,
    Bls12_381_G2_neg_memory_arguments,
    Bls12_381_G2_scalarMul_cpu_arguments_intercept,
    Bls12_381_G2_scalarMul_cpu_arguments_slope,
    Bls12_381_G2_scalarMul_memory_arguments,
    Bls12_381_G2_uncompress_cpu_arguments,
    Bls12_381_G2_uncompress_memory_arguments,
    Bls12_381_finalVerify_cpu_arguments,
    Bls12_381_finalVerify_memory_arguments,
    Bls12_381_millerLoop_cpu_arguments,
    Bls12_381_millerLoop_memory_arguments,
    Bls12_381_mulMlResult_cpu_arguments,
    Bls12_381_mulMlResult_memory_arguments,
    ByteStringToInteger_cpu_arguments_c0,
    ByteStringToInteger_cpu_arguments_c1,
    ByteStringToInteger_cpu_arguments_c2,
    ByteStringToInteger_memory_arguments_intercept,
    ByteStringToInteger_memory_arguments_slope,
    CekApplyCost_exBudgetCPU,
    CekApplyCost_exBudgetMemory,
    CekBuiltinCost_exBudgetCPU,
    CekBuiltinCost_exBudgetMemory,
    CekCaseCost_exBudgetCPU,
    CekCaseCost_exBudgetMemory,
    CekConstCost_exBudgetCPU,
    CekConstCost_exBudgetMemory,
    CekConstrCost_exBudgetCPU,
    CekConstrCost_exBudgetMemory,
    CekDelayCost_exBudgetCPU,
    CekDelayCost_exBudgetMemory,
    CekForceCost_exBudgetCPU,
    CekForceCost_exBudgetMemory,
    CekLamCost_exBudgetCPU,
    CekLamCost_exBudgetMemory,
    CekStartupCost_exBudgetCPU,
    CekStartupCost_exBudgetMemory,
    CekVarCost_exBudgetCPU,
    CekVarCost_exBudgetMemory,
    ChooseData_cpu_arguments,
    ChooseData_memory_arguments,
    ChooseList_cpu_arguments,
    ChooseList_memory_arguments,
    ChooseUnit_cpu_arguments,
    ChooseUnit_memory_arguments,
    ComplementByteString_cpu_arguments_intercept,
    ComplementByteString_cpu_arguments_slope,
    ComplementByteString_memory_arguments_intercept,
    ComplementByteString_memory_arguments_slope,
    ConsByteString_cpu_arguments_intercept,
    ConsByteString_cpu_arguments_slope,
    ConsByteString_memory_arguments_intercept,
    ConsByteString_memory_arguments_slope,
    ConstrData_cpu_arguments,
    ConstrData_memory_arguments,
    CountSetBits_cpu_arguments_intercept,
    CountSetBits_cpu_arguments_slope,
    CountSetBits_memory_arguments,
    DecodeUtf8_cpu_arguments_intercept,
    DecodeUtf8_cpu_arguments_slope,
    DecodeUtf8_memory_arguments_intercept,
    DecodeUtf8_memory_arguments_slope,
    DivideInteger_cpu_arguments_constant,
    DivideInteger_cpu_arguments_model_arguments_c00,
    DivideInteger_cpu_arguments_model_arguments_c01,
    DivideInteger_cpu_arguments_model_arguments_c02,
    DivideInteger_cpu_arguments_model_arguments_c10,
    DivideInteger_cpu_arguments_model_arguments_c11,
    DivideInteger_cpu_arguments_model_arguments_c20,
    DivideInteger_cpu_arguments_model_arguments_intercept,
    DivideInteger_cpu_arguments_model_arguments_minimum,
    DivideInteger_cpu_arguments_model_arguments_slope,
    DivideInteger_memory_arguments_intercept,
    DivideInteger_memory_arguments_minimum,
    DivideInteger_memory_arguments_slope,
    DropList_cpu_arguments_intercept,
    DropList_cpu_arguments_slope,
    DropList_memory_arguments,
    EncodeUtf8_cpu_arguments_intercept,
    EncodeUtf8_cpu_arguments_slope,
    EncodeUtf8_memory_arguments_intercept,
    EncodeUtf8_memory_arguments_slope,
    EqualsByteString_cpu_arguments_constant,
    EqualsByteString_cpu_arguments_intercept,
    EqualsByteString_cpu_arguments_slope,
    EqualsByteString_memory_arguments,
    EqualsData_cpu_arguments_intercept,
    EqualsData_cpu_arguments_slope,
    EqualsData_memory_arguments,
    EqualsInteger_cpu_arguments_intercept,
    EqualsInteger_cpu_arguments_slope,
    EqualsInteger_memory_arguments,
    EqualsString_cpu_arguments_constant,
    EqualsString_cpu_arguments_intercept,
    EqualsString_cpu_arguments_slope,
    EqualsString_memory_arguments,
    ExpModInteger_cpu_arguments_coefficient00,
    ExpModInteger_cpu_arguments_coefficient11,
    ExpModInteger_cpu_arguments_coefficient12,
    ExpModInteger_memory_arguments_intercept,
    ExpModInteger_memory_arguments_slope,
    FindFirstSetBit_cpu_arguments_intercept,
    FindFirstSetBit_cpu_arguments_slope,
    FindFirstSetBit_memory_arguments,
    FstPair_cpu_arguments,
    FstPair_memory_arguments,
    HeadList_cpu_arguments,
    HeadList_memory_arguments,
    IData_cpu_arguments,
    IData_memory_arguments,
    IfThenElse_cpu_arguments,
    IfThenElse_memory_arguments,
    IndexArray_cpu_arguments,
    IndexArray_memory_arguments,
    IndexByteString_cpu_arguments,
    IndexByteString_memory_arguments,
    InsertCoin_cpu_arguments_intercept,
    InsertCoin_cpu_arguments_slope,
    InsertCoin_memory_arguments_intercept,
    InsertCoin_memory_arguments_slope,
    IntegerToByteString_cpu_arguments_c0,
    IntegerToByteString_cpu_arguments_c1,
    IntegerToByteString_cpu_arguments_c2,
    IntegerToByteString_memory_arguments_intercept,
    IntegerToByteString_memory_arguments_slope,
    Keccak_256_cpu_arguments_intercept,
    Keccak_256_cpu_arguments_slope,
    Keccak_256_memory_arguments,
    LengthOfArray_cpu_arguments,
    LengthOfArray_memory_arguments,
    LengthOfByteString_cpu_arguments,
    LengthOfByteString_memory_arguments,
    LessThanByteString_cpu_arguments_intercept,
    LessThanByteString_cpu_arguments_slope,
    LessThanByteString_memory_arguments,
    LessThanEqualsByteString_cpu_arguments_intercept,
    LessThanEqualsByteString_cpu_arguments_slope,
    LessThanEqualsByteString_memory_arguments,
    LessThanEqualsInteger_cpu_arguments_intercept,
    LessThanEqualsInteger_cpu_arguments_slope,
    LessThanEqualsInteger_memory_arguments,
    LessThanInteger_cpu_arguments_intercept,
    LessThanInteger_cpu_arguments_slope,
    LessThanInteger_memory_arguments,
    ListData_cpu_arguments,
    ListData_memory_arguments,
    ListToArray_cpu_arguments_intercept,
    ListToArray_cpu_arguments_slope,
    ListToArray_memory_arguments_intercept,
    ListToArray_memory_arguments_slope,
    LookupCoin_cpu_arguments_intercept,
    LookupCoin_cpu_arguments_slope,
    LookupCoin_memory_arguments,
    MapData_cpu_arguments,
    MapData_memory_arguments,
    MkCons_cpu_arguments,
    MkCons_memory_arguments,
    MkNilData_cpu_arguments,
    MkNilData_memory_arguments,
    MkNilPairData_cpu_arguments,
    MkNilPairData_memory_arguments,
    MkPairData_cpu_arguments,
    MkPairData_memory_arguments,
    ModInteger_cpu_arguments_constant,
    ModInteger_cpu_arguments_model_arguments_c00,
    ModInteger_cpu_arguments_model_arguments_c01,
    ModInteger_cpu_arguments_model_arguments_c02,
    ModInteger_cpu_arguments_model_arguments_c10,
    ModInteger_cpu_arguments_model_arguments_c11,
    ModInteger_cpu_arguments_model_arguments_c20,
    ModInteger_cpu_arguments_model_arguments_intercept,
    ModInteger_cpu_arguments_model_arguments_minimum,
    ModInteger_cpu_arguments_model_arguments_slope,
    ModInteger_memory_arguments_intercept,
    ModInteger_memory_arguments_minimum,
    ModInteger_memory_arguments_slope,
    MultiplyInteger_cpu_arguments_intercept,
    MultiplyInteger_cpu_arguments_slope,
    MultiplyInteger_memory_arguments_intercept,
    MultiplyInteger_memory_arguments_slope,
    NullList_cpu_arguments,
    NullList_memory_arguments,
    OrByteString_cpu_arguments_intercept,
    OrByteString_cpu_arguments_slope1,
    OrByteString_cpu_arguments_slope2,
    OrByteString_memory_arguments_intercept,
    OrByteString_memory_arguments_slope,
    QuotientInteger_cpu_arguments_constant,
    QuotientInteger_cpu_arguments_model_arguments_c00,
    QuotientInteger_cpu_arguments_model_arguments_c01,
    QuotientInteger_cpu_arguments_model_arguments_c02,
    QuotientInteger_cpu_arguments_model_arguments_c10,
    QuotientInteger_cpu_arguments_model_arguments_c11,
    QuotientInteger_cpu_arguments_model_arguments_c20,
    QuotientInteger_cpu_arguments_model_arguments_intercept,
    QuotientInteger_cpu_arguments_model_arguments_minimum,
    QuotientInteger_cpu_arguments_model_arguments_slope,
    QuotientInteger_memory_arguments_intercept,
    QuotientInteger_memory_arguments_minimum,
    QuotientInteger_memory_arguments_slope,
    ReadBit_cpu_arguments,
    ReadBit_memory_arguments,
    RemainderInteger_cpu_arguments_constant,
    RemainderInteger_cpu_arguments_model_arguments_c00,
    RemainderInteger_cpu_arguments_model_arguments_c01,
    RemainderInteger_cpu_arguments_model_arguments_c02,
    RemainderInteger_cpu_arguments_model_arguments_c10,
    RemainderInteger_cpu_arguments_model_arguments_c11,
    RemainderInteger_cpu_arguments_model_arguments_c20,
    RemainderInteger_cpu_arguments_model_arguments_intercept,
    RemainderInteger_cpu_arguments_model_arguments_minimum,
    RemainderInteger_cpu_arguments_model_arguments_slope,
    RemainderInteger_memory_arguments_intercept,
    RemainderInteger_memory_arguments_minimum,
    RemainderInteger_memory_arguments_slope,
    ReplicateByte_cpu_arguments_intercept,
    ReplicateByte_cpu_arguments_slope,
    ReplicateByte_memory_arguments_intercept,
    ReplicateByte_memory_arguments_slope,
    Ripemd_160_cpu_arguments_intercept,
    Ripemd_160_cpu_arguments_slope,
    Ripemd_160_memory_arguments,
    RotateByteString_cpu_arguments_intercept,
    RotateByteString_cpu_arguments_slope,
    RotateByteString_memory_arguments_intercept,
    RotateByteString_memory_arguments_slope,
    ScaleValue_cpu_arguments_intercept,
    ScaleValue_cpu_arguments_slope,
    ScaleValue_memory_arguments_intercept,
    ScaleValue_memory_arguments_slope,
    SerialiseData_cpu_arguments_intercept,
    SerialiseData_cpu_arguments_slope,
    SerialiseData_memory_arguments_intercept,
    SerialiseData_memory_arguments_slope,
    Sha2_256_cpu_arguments_intercept,
    Sha2_256_cpu_arguments_slope,
    Sha2_256_memory_arguments,
    Sha3_256_cpu_arguments_intercept,
    Sha3_256_cpu_arguments_slope,
    Sha3_256_memory_arguments,
    ShiftByteString_cpu_arguments_intercept,
    ShiftByteString_cpu_arguments_slope,
    ShiftByteString_memory_arguments_intercept,
    ShiftByteString_memory_arguments_slope,
    SliceByteString_cpu_arguments_intercept,
    SliceByteString_cpu_arguments_slope,
    SliceByteString_memory_arguments_intercept,
    SliceByteString_memory_arguments_slope,
    SndPair_cpu_arguments,
    SndPair_memory_arguments,
    SubtractInteger_cpu_arguments_intercept,
    SubtractInteger_cpu_arguments_slope,
    SubtractInteger_memory_arguments_intercept,
    SubtractInteger_memory_arguments_slope,
    TailList_cpu_arguments,
    TailList_memory_arguments,
    Trace_cpu_arguments,
    Trace_memory_arguments,
    UnBData_cpu_arguments,
    UnBData_memory_arguments,
    UnConstrData_cpu_arguments,
    UnConstrData_memory_arguments,
    UnIData_cpu_arguments,
    UnIData_memory_arguments,
    UnListData_cpu_arguments,
    UnListData_memory_arguments,
    UnMapData_cpu_arguments,
    UnMapData_memory_arguments,
    UnValueData_cpu_arguments_c0,
    UnValueData_cpu_arguments_c1,
    UnValueData_cpu_arguments_c2,
    UnValueData_memory_arguments_intercept,
    UnValueData_memory_arguments_slope,
    UnionValue_cpu_arguments_c00,
    UnionValue_cpu_arguments_c01,
    UnionValue_cpu_arguments_c10,
    UnionValue_cpu_arguments_c11,
    UnionValue_memory_arguments_intercept,
    UnionValue_memory_arguments_slope,
    ValueContains_cpu_arguments_constant,
    ValueContains_cpu_arguments_model_arguments_intercept,
    ValueContains_cpu_arguments_model_arguments_slope1,
    ValueContains_cpu_arguments_model_arguments_slope2,
    ValueContains_memory_arguments,
    ValueData_cpu_arguments_intercept,
    ValueData_cpu_arguments_slope,
    ValueData_memory_arguments_intercept,
    ValueData_memory_arguments_slope,
    VerifyEcdsaSecp256k1Signature_cpu_arguments,
    VerifyEcdsaSecp256k1Signature_memory_arguments,
    VerifyEd25519Signature_cpu_arguments_intercept,
    VerifyEd25519Signature_cpu_arguments_slope,
    VerifyEd25519Signature_memory_arguments,
    VerifySchnorrSecp256k1Signature_cpu_arguments_intercept,
    VerifySchnorrSecp256k1Signature_cpu_arguments_slope,
    VerifySchnorrSecp256k1Signature_memory_arguments,
    WriteBits_cpu_arguments_intercept,
    WriteBits_cpu_arguments_slope,
    WriteBits_memory_arguments_intercept,
    WriteBits_memory_arguments_slope,
    XorByteString_cpu_arguments_intercept,
    XorByteString_cpu_arguments_slope1,
    XorByteString_cpu_arguments_slope2,
    XorByteString_memory_arguments_intercept,
    XorByteString_memory_arguments_slope,
}

impl ParamName {
    /// Order (and which) in which parameters are defined in V1.
    pub const V1: [ParamName; 332] = [
        Self::AddInteger_cpu_arguments_intercept,
        Self::AddInteger_cpu_arguments_slope,
        Self::AddInteger_memory_arguments_intercept,
        Self::AddInteger_memory_arguments_slope,
        Self::AppendByteString_cpu_arguments_intercept,
        Self::AppendByteString_cpu_arguments_slope,
        Self::AppendByteString_memory_arguments_intercept,
        Self::AppendByteString_memory_arguments_slope,
        Self::AppendString_cpu_arguments_intercept,
        Self::AppendString_cpu_arguments_slope,
        Self::AppendString_memory_arguments_intercept,
        Self::AppendString_memory_arguments_slope,
        Self::BData_cpu_arguments,
        Self::BData_memory_arguments,
        Self::Blake2b_256_cpu_arguments_intercept,
        Self::Blake2b_256_cpu_arguments_slope,
        Self::Blake2b_256_memory_arguments,
        Self::CekApplyCost_exBudgetCPU,
        Self::CekApplyCost_exBudgetMemory,
        Self::CekBuiltinCost_exBudgetCPU,
        Self::CekBuiltinCost_exBudgetMemory,
        Self::CekConstCost_exBudgetCPU,
        Self::CekConstCost_exBudgetMemory,
        Self::CekDelayCost_exBudgetCPU,
        Self::CekDelayCost_exBudgetMemory,
        Self::CekForceCost_exBudgetCPU,
        Self::CekForceCost_exBudgetMemory,
        Self::CekLamCost_exBudgetCPU,
        Self::CekLamCost_exBudgetMemory,
        Self::CekStartupCost_exBudgetCPU,
        Self::CekStartupCost_exBudgetMemory,
        Self::CekVarCost_exBudgetCPU,
        Self::CekVarCost_exBudgetMemory,
        Self::ChooseData_cpu_arguments,
        Self::ChooseData_memory_arguments,
        Self::ChooseList_cpu_arguments,
        Self::ChooseList_memory_arguments,
        Self::ChooseUnit_cpu_arguments,
        Self::ChooseUnit_memory_arguments,
        Self::ConsByteString_cpu_arguments_intercept,
        Self::ConsByteString_cpu_arguments_slope,
        Self::ConsByteString_memory_arguments_intercept,
        Self::ConsByteString_memory_arguments_slope,
        Self::ConstrData_cpu_arguments,
        Self::ConstrData_memory_arguments,
        Self::DecodeUtf8_cpu_arguments_intercept,
        Self::DecodeUtf8_cpu_arguments_slope,
        Self::DecodeUtf8_memory_arguments_intercept,
        Self::DecodeUtf8_memory_arguments_slope,
        Self::DivideInteger_cpu_arguments_constant,
        Self::DivideInteger_cpu_arguments_model_arguments_intercept,
        Self::DivideInteger_cpu_arguments_model_arguments_slope,
        Self::DivideInteger_memory_arguments_intercept,
        Self::DivideInteger_memory_arguments_minimum,
        Self::DivideInteger_memory_arguments_slope,
        Self::EncodeUtf8_cpu_arguments_intercept,
        Self::EncodeUtf8_cpu_arguments_slope,
        Self::EncodeUtf8_memory_arguments_intercept,
        Self::EncodeUtf8_memory_arguments_slope,
        Self::EqualsByteString_cpu_arguments_constant,
        Self::EqualsByteString_cpu_arguments_intercept,
        Self::EqualsByteString_cpu_arguments_slope,
        Self::EqualsByteString_memory_arguments,
        Self::EqualsData_cpu_arguments_intercept,
        Self::EqualsData_cpu_arguments_slope,
        Self::EqualsData_memory_arguments,
        Self::EqualsInteger_cpu_arguments_intercept,
        Self::EqualsInteger_cpu_arguments_slope,
        Self::EqualsInteger_memory_arguments,
        Self::EqualsString_cpu_arguments_constant,
        Self::EqualsString_cpu_arguments_intercept,
        Self::EqualsString_cpu_arguments_slope,
        Self::EqualsString_memory_arguments,
        Self::FstPair_cpu_arguments,
        Self::FstPair_memory_arguments,
        Self::HeadList_cpu_arguments,
        Self::HeadList_memory_arguments,
        Self::IData_cpu_arguments,
        Self::IData_memory_arguments,
        Self::IfThenElse_cpu_arguments,
        Self::IfThenElse_memory_arguments,
        Self::IndexByteString_cpu_arguments,
        Self::IndexByteString_memory_arguments,
        Self::LengthOfByteString_cpu_arguments,
        Self::LengthOfByteString_memory_arguments,
        Self::LessThanByteString_cpu_arguments_intercept,
        Self::LessThanByteString_cpu_arguments_slope,
        Self::LessThanByteString_memory_arguments,
        Self::LessThanEqualsByteString_cpu_arguments_intercept,
        Self::LessThanEqualsByteString_cpu_arguments_slope,
        Self::LessThanEqualsByteString_memory_arguments,
        Self::LessThanEqualsInteger_cpu_arguments_intercept,
        Self::LessThanEqualsInteger_cpu_arguments_slope,
        Self::LessThanEqualsInteger_memory_arguments,
        Self::LessThanInteger_cpu_arguments_intercept,
        Self::LessThanInteger_cpu_arguments_slope,
        Self::LessThanInteger_memory_arguments,
        Self::ListData_cpu_arguments,
        Self::ListData_memory_arguments,
        Self::MapData_cpu_arguments,
        Self::MapData_memory_arguments,
        Self::MkCons_cpu_arguments,
        Self::MkCons_memory_arguments,
        Self::MkNilData_cpu_arguments,
        Self::MkNilData_memory_arguments,
        Self::MkNilPairData_cpu_arguments,
        Self::MkNilPairData_memory_arguments,
        Self::MkPairData_cpu_arguments,
        Self::MkPairData_memory_arguments,
        Self::ModInteger_cpu_arguments_constant,
        Self::ModInteger_cpu_arguments_model_arguments_intercept,
        Self::ModInteger_cpu_arguments_model_arguments_slope,
        Self::ModInteger_memory_arguments_intercept,
        Self::ModInteger_memory_arguments_minimum,
        Self::ModInteger_memory_arguments_slope,
        Self::MultiplyInteger_cpu_arguments_intercept,
        Self::MultiplyInteger_cpu_arguments_slope,
        Self::MultiplyInteger_memory_arguments_intercept,
        Self::MultiplyInteger_memory_arguments_slope,
        Self::NullList_cpu_arguments,
        Self::NullList_memory_arguments,
        Self::QuotientInteger_cpu_arguments_constant,
        Self::QuotientInteger_cpu_arguments_model_arguments_intercept,
        Self::QuotientInteger_cpu_arguments_model_arguments_slope,
        Self::QuotientInteger_memory_arguments_intercept,
        Self::QuotientInteger_memory_arguments_minimum,
        Self::QuotientInteger_memory_arguments_slope,
        Self::RemainderInteger_cpu_arguments_constant,
        Self::RemainderInteger_cpu_arguments_model_arguments_intercept,
        Self::RemainderInteger_cpu_arguments_model_arguments_slope,
        Self::RemainderInteger_memory_arguments_intercept,
        Self::RemainderInteger_memory_arguments_minimum,
        Self::RemainderInteger_memory_arguments_slope,
        Self::Sha2_256_cpu_arguments_intercept,
        Self::Sha2_256_cpu_arguments_slope,
        Self::Sha2_256_memory_arguments,
        Self::Sha3_256_cpu_arguments_intercept,
        Self::Sha3_256_cpu_arguments_slope,
        Self::Sha3_256_memory_arguments,
        Self::SliceByteString_cpu_arguments_intercept,
        Self::SliceByteString_cpu_arguments_slope,
        Self::SliceByteString_memory_arguments_intercept,
        Self::SliceByteString_memory_arguments_slope,
        Self::SndPair_cpu_arguments,
        Self::SndPair_memory_arguments,
        Self::SubtractInteger_cpu_arguments_intercept,
        Self::SubtractInteger_cpu_arguments_slope,
        Self::SubtractInteger_memory_arguments_intercept,
        Self::SubtractInteger_memory_arguments_slope,
        Self::TailList_cpu_arguments,
        Self::TailList_memory_arguments,
        Self::Trace_cpu_arguments,
        Self::Trace_memory_arguments,
        Self::UnBData_cpu_arguments,
        Self::UnBData_memory_arguments,
        Self::UnConstrData_cpu_arguments,
        Self::UnConstrData_memory_arguments,
        Self::UnIData_cpu_arguments,
        Self::UnIData_memory_arguments,
        Self::UnListData_cpu_arguments,
        Self::UnListData_memory_arguments,
        Self::UnMapData_cpu_arguments,
        Self::UnMapData_memory_arguments,
        Self::VerifyEd25519Signature_cpu_arguments_intercept,
        Self::VerifyEd25519Signature_cpu_arguments_slope,
        Self::VerifyEd25519Signature_memory_arguments,
        Self::SerialiseData_cpu_arguments_intercept,
        Self::SerialiseData_cpu_arguments_slope,
        Self::SerialiseData_memory_arguments_intercept,
        Self::SerialiseData_memory_arguments_slope,
        Self::VerifyEcdsaSecp256k1Signature_cpu_arguments,
        Self::VerifyEcdsaSecp256k1Signature_memory_arguments,
        Self::VerifySchnorrSecp256k1Signature_cpu_arguments_intercept,
        Self::VerifySchnorrSecp256k1Signature_cpu_arguments_slope,
        Self::VerifySchnorrSecp256k1Signature_memory_arguments,
        Self::CekConstrCost_exBudgetCPU,
        Self::CekConstrCost_exBudgetMemory,
        Self::CekCaseCost_exBudgetCPU,
        Self::CekCaseCost_exBudgetMemory,
        Self::Bls12_381_G1_add_cpu_arguments,
        Self::Bls12_381_G1_add_memory_arguments,
        Self::Bls12_381_G1_compress_cpu_arguments,
        Self::Bls12_381_G1_compress_memory_arguments,
        Self::Bls12_381_G1_equal_cpu_arguments,
        Self::Bls12_381_G1_equal_memory_arguments,
        Self::Bls12_381_G1_hashToGroup_cpu_arguments_intercept,
        Self::Bls12_381_G1_hashToGroup_cpu_arguments_slope,
        Self::Bls12_381_G1_hashToGroup_memory_arguments,
        Self::Bls12_381_G1_neg_cpu_arguments,
        Self::Bls12_381_G1_neg_memory_arguments,
        Self::Bls12_381_G1_scalarMul_cpu_arguments_intercept,
        Self::Bls12_381_G1_scalarMul_cpu_arguments_slope,
        Self::Bls12_381_G1_scalarMul_memory_arguments,
        Self::Bls12_381_G1_uncompress_cpu_arguments,
        Self::Bls12_381_G1_uncompress_memory_arguments,
        Self::Bls12_381_G2_add_cpu_arguments,
        Self::Bls12_381_G2_add_memory_arguments,
        Self::Bls12_381_G2_compress_cpu_arguments,
        Self::Bls12_381_G2_compress_memory_arguments,
        Self::Bls12_381_G2_equal_cpu_arguments,
        Self::Bls12_381_G2_equal_memory_arguments,
        Self::Bls12_381_G2_hashToGroup_cpu_arguments_intercept,
        Self::Bls12_381_G2_hashToGroup_cpu_arguments_slope,
        Self::Bls12_381_G2_hashToGroup_memory_arguments,
        Self::Bls12_381_G2_neg_cpu_arguments,
        Self::Bls12_381_G2_neg_memory_arguments,
        Self::Bls12_381_G2_scalarMul_cpu_arguments_intercept,
        Self::Bls12_381_G2_scalarMul_cpu_arguments_slope,
        Self::Bls12_381_G2_scalarMul_memory_arguments,
        Self::Bls12_381_G2_uncompress_cpu_arguments,
        Self::Bls12_381_G2_uncompress_memory_arguments,
        Self::Bls12_381_finalVerify_cpu_arguments,
        Self::Bls12_381_finalVerify_memory_arguments,
        Self::Bls12_381_millerLoop_cpu_arguments,
        Self::Bls12_381_millerLoop_memory_arguments,
        Self::Bls12_381_mulMlResult_cpu_arguments,
        Self::Bls12_381_mulMlResult_memory_arguments,
        Self::Keccak_256_cpu_arguments_intercept,
        Self::Keccak_256_cpu_arguments_slope,
        Self::Keccak_256_memory_arguments,
        Self::Blake2b_224_cpu_arguments_intercept,
        Self::Blake2b_224_cpu_arguments_slope,
        Self::Blake2b_224_memory_arguments,
        Self::IntegerToByteString_cpu_arguments_c0,
        Self::IntegerToByteString_cpu_arguments_c1,
        Self::IntegerToByteString_cpu_arguments_c2,
        Self::IntegerToByteString_memory_arguments_intercept,
        Self::IntegerToByteString_memory_arguments_slope,
        Self::ByteStringToInteger_cpu_arguments_c0,
        Self::ByteStringToInteger_cpu_arguments_c1,
        Self::ByteStringToInteger_cpu_arguments_c2,
        Self::ByteStringToInteger_memory_arguments_intercept,
        Self::ByteStringToInteger_memory_arguments_slope,
        Self::AndByteString_cpu_arguments_intercept,
        Self::AndByteString_cpu_arguments_slope1,
        Self::AndByteString_cpu_arguments_slope2,
        Self::AndByteString_memory_arguments_intercept,
        Self::AndByteString_memory_arguments_slope,
        Self::OrByteString_cpu_arguments_intercept,
        Self::OrByteString_cpu_arguments_slope1,
        Self::OrByteString_cpu_arguments_slope2,
        Self::OrByteString_memory_arguments_intercept,
        Self::OrByteString_memory_arguments_slope,
        Self::XorByteString_cpu_arguments_intercept,
        Self::XorByteString_cpu_arguments_slope1,
        Self::XorByteString_cpu_arguments_slope2,
        Self::XorByteString_memory_arguments_intercept,
        Self::XorByteString_memory_arguments_slope,
        Self::ComplementByteString_cpu_arguments_intercept,
        Self::ComplementByteString_cpu_arguments_slope,
        Self::ComplementByteString_memory_arguments_intercept,
        Self::ComplementByteString_memory_arguments_slope,
        Self::ReadBit_cpu_arguments,
        Self::ReadBit_memory_arguments,
        Self::WriteBits_cpu_arguments_intercept,
        Self::WriteBits_cpu_arguments_slope,
        Self::WriteBits_memory_arguments_intercept,
        Self::WriteBits_memory_arguments_slope,
        Self::ReplicateByte_cpu_arguments_intercept,
        Self::ReplicateByte_cpu_arguments_slope,
        Self::ReplicateByte_memory_arguments_intercept,
        Self::ReplicateByte_memory_arguments_slope,
        Self::ShiftByteString_cpu_arguments_intercept,
        Self::ShiftByteString_cpu_arguments_slope,
        Self::ShiftByteString_memory_arguments_intercept,
        Self::ShiftByteString_memory_arguments_slope,
        Self::RotateByteString_cpu_arguments_intercept,
        Self::RotateByteString_cpu_arguments_slope,
        Self::RotateByteString_memory_arguments_intercept,
        Self::RotateByteString_memory_arguments_slope,
        Self::CountSetBits_cpu_arguments_intercept,
        Self::CountSetBits_cpu_arguments_slope,
        Self::CountSetBits_memory_arguments,
        Self::FindFirstSetBit_cpu_arguments_intercept,
        Self::FindFirstSetBit_cpu_arguments_slope,
        Self::FindFirstSetBit_memory_arguments,
        Self::Ripemd_160_cpu_arguments_intercept,
        Self::Ripemd_160_cpu_arguments_slope,
        Self::Ripemd_160_memory_arguments,
        Self::ExpModInteger_cpu_arguments_coefficient00,
        Self::ExpModInteger_cpu_arguments_coefficient11,
        Self::ExpModInteger_cpu_arguments_coefficient12,
        Self::ExpModInteger_memory_arguments_intercept,
        Self::ExpModInteger_memory_arguments_slope,
        Self::DropList_cpu_arguments_intercept,
        Self::DropList_cpu_arguments_slope,
        Self::DropList_memory_arguments,
        Self::LengthOfArray_cpu_arguments,
        Self::LengthOfArray_memory_arguments,
        Self::ListToArray_cpu_arguments_intercept,
        Self::ListToArray_cpu_arguments_slope,
        Self::ListToArray_memory_arguments_intercept,
        Self::ListToArray_memory_arguments_slope,
        Self::IndexArray_cpu_arguments,
        Self::IndexArray_memory_arguments,
        Self::Bls12_381_G1_multiScalarMul_cpu_arguments_intercept,
        Self::Bls12_381_G1_multiScalarMul_cpu_arguments_slope,
        Self::Bls12_381_G1_multiScalarMul_memory_arguments,
        Self::Bls12_381_G2_multiScalarMul_cpu_arguments_intercept,
        Self::Bls12_381_G2_multiScalarMul_cpu_arguments_slope,
        Self::Bls12_381_G2_multiScalarMul_memory_arguments,
        Self::InsertCoin_cpu_arguments_intercept,
        Self::InsertCoin_cpu_arguments_slope,
        Self::InsertCoin_memory_arguments_intercept,
        Self::InsertCoin_memory_arguments_slope,
        Self::LookupCoin_cpu_arguments_intercept,
        Self::LookupCoin_cpu_arguments_slope,
        Self::LookupCoin_memory_arguments,
        Self::UnionValue_cpu_arguments_c00,
        Self::UnionValue_cpu_arguments_c10,
        Self::UnionValue_cpu_arguments_c01,
        Self::UnionValue_cpu_arguments_c11,
        Self::UnionValue_memory_arguments_intercept,
        Self::UnionValue_memory_arguments_slope,
        Self::ValueContains_cpu_arguments_constant,
        Self::ValueContains_cpu_arguments_model_arguments_intercept,
        Self::ValueContains_cpu_arguments_model_arguments_slope1,
        Self::ValueContains_cpu_arguments_model_arguments_slope2,
        Self::ValueContains_memory_arguments,
        Self::ValueData_cpu_arguments_intercept,
        Self::ValueData_cpu_arguments_slope,
        Self::ValueData_memory_arguments_intercept,
        Self::ValueData_memory_arguments_slope,
        Self::UnValueData_cpu_arguments_c0,
        Self::UnValueData_cpu_arguments_c1,
        Self::UnValueData_cpu_arguments_c2,
        Self::UnValueData_memory_arguments_intercept,
        Self::UnValueData_memory_arguments_slope,
        Self::ScaleValue_cpu_arguments_intercept,
        Self::ScaleValue_cpu_arguments_slope,
        Self::ScaleValue_memory_arguments_intercept,
        Self::ScaleValue_memory_arguments_slope,
    ];

    /// Order (and which) in which parameters are defined in V1.
    pub const V2: [Self; 332] = [
        Self::AddInteger_cpu_arguments_intercept,
        Self::AddInteger_cpu_arguments_slope,
        Self::AddInteger_memory_arguments_intercept,
        Self::AddInteger_memory_arguments_slope,
        Self::AppendByteString_cpu_arguments_intercept,
        Self::AppendByteString_cpu_arguments_slope,
        Self::AppendByteString_memory_arguments_intercept,
        Self::AppendByteString_memory_arguments_slope,
        Self::AppendString_cpu_arguments_intercept,
        Self::AppendString_cpu_arguments_slope,
        Self::AppendString_memory_arguments_intercept,
        Self::AppendString_memory_arguments_slope,
        Self::BData_cpu_arguments,
        Self::BData_memory_arguments,
        Self::Blake2b_256_cpu_arguments_intercept,
        Self::Blake2b_256_cpu_arguments_slope,
        Self::Blake2b_256_memory_arguments,
        Self::CekApplyCost_exBudgetCPU,
        Self::CekApplyCost_exBudgetMemory,
        Self::CekBuiltinCost_exBudgetCPU,
        Self::CekBuiltinCost_exBudgetMemory,
        Self::CekConstCost_exBudgetCPU,
        Self::CekConstCost_exBudgetMemory,
        Self::CekDelayCost_exBudgetCPU,
        Self::CekDelayCost_exBudgetMemory,
        Self::CekForceCost_exBudgetCPU,
        Self::CekForceCost_exBudgetMemory,
        Self::CekLamCost_exBudgetCPU,
        Self::CekLamCost_exBudgetMemory,
        Self::CekStartupCost_exBudgetCPU,
        Self::CekStartupCost_exBudgetMemory,
        Self::CekVarCost_exBudgetCPU,
        Self::CekVarCost_exBudgetMemory,
        Self::ChooseData_cpu_arguments,
        Self::ChooseData_memory_arguments,
        Self::ChooseList_cpu_arguments,
        Self::ChooseList_memory_arguments,
        Self::ChooseUnit_cpu_arguments,
        Self::ChooseUnit_memory_arguments,
        Self::ConsByteString_cpu_arguments_intercept,
        Self::ConsByteString_cpu_arguments_slope,
        Self::ConsByteString_memory_arguments_intercept,
        Self::ConsByteString_memory_arguments_slope,
        Self::ConstrData_cpu_arguments,
        Self::ConstrData_memory_arguments,
        Self::DecodeUtf8_cpu_arguments_intercept,
        Self::DecodeUtf8_cpu_arguments_slope,
        Self::DecodeUtf8_memory_arguments_intercept,
        Self::DecodeUtf8_memory_arguments_slope,
        Self::DivideInteger_cpu_arguments_constant,
        Self::DivideInteger_cpu_arguments_model_arguments_intercept,
        Self::DivideInteger_cpu_arguments_model_arguments_slope,
        Self::DivideInteger_memory_arguments_intercept,
        Self::DivideInteger_memory_arguments_minimum,
        Self::DivideInteger_memory_arguments_slope,
        Self::EncodeUtf8_cpu_arguments_intercept,
        Self::EncodeUtf8_cpu_arguments_slope,
        Self::EncodeUtf8_memory_arguments_intercept,
        Self::EncodeUtf8_memory_arguments_slope,
        Self::EqualsByteString_cpu_arguments_constant,
        Self::EqualsByteString_cpu_arguments_intercept,
        Self::EqualsByteString_cpu_arguments_slope,
        Self::EqualsByteString_memory_arguments,
        Self::EqualsData_cpu_arguments_intercept,
        Self::EqualsData_cpu_arguments_slope,
        Self::EqualsData_memory_arguments,
        Self::EqualsInteger_cpu_arguments_intercept,
        Self::EqualsInteger_cpu_arguments_slope,
        Self::EqualsInteger_memory_arguments,
        Self::EqualsString_cpu_arguments_constant,
        Self::EqualsString_cpu_arguments_intercept,
        Self::EqualsString_cpu_arguments_slope,
        Self::EqualsString_memory_arguments,
        Self::FstPair_cpu_arguments,
        Self::FstPair_memory_arguments,
        Self::HeadList_cpu_arguments,
        Self::HeadList_memory_arguments,
        Self::IData_cpu_arguments,
        Self::IData_memory_arguments,
        Self::IfThenElse_cpu_arguments,
        Self::IfThenElse_memory_arguments,
        Self::IndexByteString_cpu_arguments,
        Self::IndexByteString_memory_arguments,
        Self::LengthOfByteString_cpu_arguments,
        Self::LengthOfByteString_memory_arguments,
        Self::LessThanByteString_cpu_arguments_intercept,
        Self::LessThanByteString_cpu_arguments_slope,
        Self::LessThanByteString_memory_arguments,
        Self::LessThanEqualsByteString_cpu_arguments_intercept,
        Self::LessThanEqualsByteString_cpu_arguments_slope,
        Self::LessThanEqualsByteString_memory_arguments,
        Self::LessThanEqualsInteger_cpu_arguments_intercept,
        Self::LessThanEqualsInteger_cpu_arguments_slope,
        Self::LessThanEqualsInteger_memory_arguments,
        Self::LessThanInteger_cpu_arguments_intercept,
        Self::LessThanInteger_cpu_arguments_slope,
        Self::LessThanInteger_memory_arguments,
        Self::ListData_cpu_arguments,
        Self::ListData_memory_arguments,
        Self::MapData_cpu_arguments,
        Self::MapData_memory_arguments,
        Self::MkCons_cpu_arguments,
        Self::MkCons_memory_arguments,
        Self::MkNilData_cpu_arguments,
        Self::MkNilData_memory_arguments,
        Self::MkNilPairData_cpu_arguments,
        Self::MkNilPairData_memory_arguments,
        Self::MkPairData_cpu_arguments,
        Self::MkPairData_memory_arguments,
        Self::ModInteger_cpu_arguments_constant,
        Self::ModInteger_cpu_arguments_model_arguments_intercept,
        Self::ModInteger_cpu_arguments_model_arguments_slope,
        Self::ModInteger_memory_arguments_intercept,
        Self::ModInteger_memory_arguments_minimum,
        Self::ModInteger_memory_arguments_slope,
        Self::MultiplyInteger_cpu_arguments_intercept,
        Self::MultiplyInteger_cpu_arguments_slope,
        Self::MultiplyInteger_memory_arguments_intercept,
        Self::MultiplyInteger_memory_arguments_slope,
        Self::NullList_cpu_arguments,
        Self::NullList_memory_arguments,
        Self::QuotientInteger_cpu_arguments_constant,
        Self::QuotientInteger_cpu_arguments_model_arguments_intercept,
        Self::QuotientInteger_cpu_arguments_model_arguments_slope,
        Self::QuotientInteger_memory_arguments_intercept,
        Self::QuotientInteger_memory_arguments_minimum,
        Self::QuotientInteger_memory_arguments_slope,
        Self::RemainderInteger_cpu_arguments_constant,
        Self::RemainderInteger_cpu_arguments_model_arguments_intercept,
        Self::RemainderInteger_cpu_arguments_model_arguments_slope,
        Self::RemainderInteger_memory_arguments_intercept,
        Self::RemainderInteger_memory_arguments_minimum,
        Self::RemainderInteger_memory_arguments_slope,
        Self::SerialiseData_cpu_arguments_intercept,
        Self::SerialiseData_cpu_arguments_slope,
        Self::SerialiseData_memory_arguments_intercept,
        Self::SerialiseData_memory_arguments_slope,
        Self::Sha2_256_cpu_arguments_intercept,
        Self::Sha2_256_cpu_arguments_slope,
        Self::Sha2_256_memory_arguments,
        Self::Sha3_256_cpu_arguments_intercept,
        Self::Sha3_256_cpu_arguments_slope,
        Self::Sha3_256_memory_arguments,
        Self::SliceByteString_cpu_arguments_intercept,
        Self::SliceByteString_cpu_arguments_slope,
        Self::SliceByteString_memory_arguments_intercept,
        Self::SliceByteString_memory_arguments_slope,
        Self::SndPair_cpu_arguments,
        Self::SndPair_memory_arguments,
        Self::SubtractInteger_cpu_arguments_intercept,
        Self::SubtractInteger_cpu_arguments_slope,
        Self::SubtractInteger_memory_arguments_intercept,
        Self::SubtractInteger_memory_arguments_slope,
        Self::TailList_cpu_arguments,
        Self::TailList_memory_arguments,
        Self::Trace_cpu_arguments,
        Self::Trace_memory_arguments,
        Self::UnBData_cpu_arguments,
        Self::UnBData_memory_arguments,
        Self::UnConstrData_cpu_arguments,
        Self::UnConstrData_memory_arguments,
        Self::UnIData_cpu_arguments,
        Self::UnIData_memory_arguments,
        Self::UnListData_cpu_arguments,
        Self::UnListData_memory_arguments,
        Self::UnMapData_cpu_arguments,
        Self::UnMapData_memory_arguments,
        Self::VerifyEcdsaSecp256k1Signature_cpu_arguments,
        Self::VerifyEcdsaSecp256k1Signature_memory_arguments,
        Self::VerifyEd25519Signature_cpu_arguments_intercept,
        Self::VerifyEd25519Signature_cpu_arguments_slope,
        Self::VerifyEd25519Signature_memory_arguments,
        Self::VerifySchnorrSecp256k1Signature_cpu_arguments_intercept,
        Self::VerifySchnorrSecp256k1Signature_cpu_arguments_slope,
        Self::VerifySchnorrSecp256k1Signature_memory_arguments,
        Self::IntegerToByteString_cpu_arguments_c0,
        Self::IntegerToByteString_cpu_arguments_c1,
        Self::IntegerToByteString_cpu_arguments_c2,
        Self::IntegerToByteString_memory_arguments_intercept,
        Self::IntegerToByteString_memory_arguments_slope,
        Self::ByteStringToInteger_cpu_arguments_c0,
        Self::ByteStringToInteger_cpu_arguments_c1,
        Self::ByteStringToInteger_cpu_arguments_c2,
        Self::ByteStringToInteger_memory_arguments_intercept,
        Self::ByteStringToInteger_memory_arguments_slope,
        Self::CekConstrCost_exBudgetCPU,
        Self::CekConstrCost_exBudgetMemory,
        Self::CekCaseCost_exBudgetCPU,
        Self::CekCaseCost_exBudgetMemory,
        Self::Bls12_381_G1_add_cpu_arguments,
        Self::Bls12_381_G1_add_memory_arguments,
        Self::Bls12_381_G1_compress_cpu_arguments,
        Self::Bls12_381_G1_compress_memory_arguments,
        Self::Bls12_381_G1_equal_cpu_arguments,
        Self::Bls12_381_G1_equal_memory_arguments,
        Self::Bls12_381_G1_hashToGroup_cpu_arguments_intercept,
        Self::Bls12_381_G1_hashToGroup_cpu_arguments_slope,
        Self::Bls12_381_G1_hashToGroup_memory_arguments,
        Self::Bls12_381_G1_neg_cpu_arguments,
        Self::Bls12_381_G1_neg_memory_arguments,
        Self::Bls12_381_G1_scalarMul_cpu_arguments_intercept,
        Self::Bls12_381_G1_scalarMul_cpu_arguments_slope,
        Self::Bls12_381_G1_scalarMul_memory_arguments,
        Self::Bls12_381_G1_uncompress_cpu_arguments,
        Self::Bls12_381_G1_uncompress_memory_arguments,
        Self::Bls12_381_G2_add_cpu_arguments,
        Self::Bls12_381_G2_add_memory_arguments,
        Self::Bls12_381_G2_compress_cpu_arguments,
        Self::Bls12_381_G2_compress_memory_arguments,
        Self::Bls12_381_G2_equal_cpu_arguments,
        Self::Bls12_381_G2_equal_memory_arguments,
        Self::Bls12_381_G2_hashToGroup_cpu_arguments_intercept,
        Self::Bls12_381_G2_hashToGroup_cpu_arguments_slope,
        Self::Bls12_381_G2_hashToGroup_memory_arguments,
        Self::Bls12_381_G2_neg_cpu_arguments,
        Self::Bls12_381_G2_neg_memory_arguments,
        Self::Bls12_381_G2_scalarMul_cpu_arguments_intercept,
        Self::Bls12_381_G2_scalarMul_cpu_arguments_slope,
        Self::Bls12_381_G2_scalarMul_memory_arguments,
        Self::Bls12_381_G2_uncompress_cpu_arguments,
        Self::Bls12_381_G2_uncompress_memory_arguments,
        Self::Bls12_381_finalVerify_cpu_arguments,
        Self::Bls12_381_finalVerify_memory_arguments,
        Self::Bls12_381_millerLoop_cpu_arguments,
        Self::Bls12_381_millerLoop_memory_arguments,
        Self::Bls12_381_mulMlResult_cpu_arguments,
        Self::Bls12_381_mulMlResult_memory_arguments,
        Self::Keccak_256_cpu_arguments_intercept,
        Self::Keccak_256_cpu_arguments_slope,
        Self::Keccak_256_memory_arguments,
        Self::Blake2b_224_cpu_arguments_intercept,
        Self::Blake2b_224_cpu_arguments_slope,
        Self::Blake2b_224_memory_arguments,
        Self::AndByteString_cpu_arguments_intercept,
        Self::AndByteString_cpu_arguments_slope1,
        Self::AndByteString_cpu_arguments_slope2,
        Self::AndByteString_memory_arguments_intercept,
        Self::AndByteString_memory_arguments_slope,
        Self::OrByteString_cpu_arguments_intercept,
        Self::OrByteString_cpu_arguments_slope1,
        Self::OrByteString_cpu_arguments_slope2,
        Self::OrByteString_memory_arguments_intercept,
        Self::OrByteString_memory_arguments_slope,
        Self::XorByteString_cpu_arguments_intercept,
        Self::XorByteString_cpu_arguments_slope1,
        Self::XorByteString_cpu_arguments_slope2,
        Self::XorByteString_memory_arguments_intercept,
        Self::XorByteString_memory_arguments_slope,
        Self::ComplementByteString_cpu_arguments_intercept,
        Self::ComplementByteString_cpu_arguments_slope,
        Self::ComplementByteString_memory_arguments_intercept,
        Self::ComplementByteString_memory_arguments_slope,
        Self::ReadBit_cpu_arguments,
        Self::ReadBit_memory_arguments,
        Self::WriteBits_cpu_arguments_intercept,
        Self::WriteBits_cpu_arguments_slope,
        Self::WriteBits_memory_arguments_intercept,
        Self::WriteBits_memory_arguments_slope,
        Self::ReplicateByte_cpu_arguments_intercept,
        Self::ReplicateByte_cpu_arguments_slope,
        Self::ReplicateByte_memory_arguments_intercept,
        Self::ReplicateByte_memory_arguments_slope,
        Self::ShiftByteString_cpu_arguments_intercept,
        Self::ShiftByteString_cpu_arguments_slope,
        Self::ShiftByteString_memory_arguments_intercept,
        Self::ShiftByteString_memory_arguments_slope,
        Self::RotateByteString_cpu_arguments_intercept,
        Self::RotateByteString_cpu_arguments_slope,
        Self::RotateByteString_memory_arguments_intercept,
        Self::RotateByteString_memory_arguments_slope,
        Self::CountSetBits_cpu_arguments_intercept,
        Self::CountSetBits_cpu_arguments_slope,
        Self::CountSetBits_memory_arguments,
        Self::FindFirstSetBit_cpu_arguments_intercept,
        Self::FindFirstSetBit_cpu_arguments_slope,
        Self::FindFirstSetBit_memory_arguments,
        Self::Ripemd_160_cpu_arguments_intercept,
        Self::Ripemd_160_cpu_arguments_slope,
        Self::Ripemd_160_memory_arguments,
        Self::ExpModInteger_cpu_arguments_coefficient00,
        Self::ExpModInteger_cpu_arguments_coefficient11,
        Self::ExpModInteger_cpu_arguments_coefficient12,
        Self::ExpModInteger_memory_arguments_intercept,
        Self::ExpModInteger_memory_arguments_slope,
        Self::DropList_cpu_arguments_intercept,
        Self::DropList_cpu_arguments_slope,
        Self::DropList_memory_arguments,
        Self::LengthOfArray_cpu_arguments,
        Self::LengthOfArray_memory_arguments,
        Self::ListToArray_cpu_arguments_intercept,
        Self::ListToArray_cpu_arguments_slope,
        Self::ListToArray_memory_arguments_intercept,
        Self::ListToArray_memory_arguments_slope,
        Self::IndexArray_cpu_arguments,
        Self::IndexArray_memory_arguments,
        Self::Bls12_381_G1_multiScalarMul_cpu_arguments_intercept,
        Self::Bls12_381_G1_multiScalarMul_cpu_arguments_slope,
        Self::Bls12_381_G1_multiScalarMul_memory_arguments,
        Self::Bls12_381_G2_multiScalarMul_cpu_arguments_intercept,
        Self::Bls12_381_G2_multiScalarMul_cpu_arguments_slope,
        Self::Bls12_381_G2_multiScalarMul_memory_arguments,
        Self::InsertCoin_cpu_arguments_intercept,
        Self::InsertCoin_cpu_arguments_slope,
        Self::InsertCoin_memory_arguments_intercept,
        Self::InsertCoin_memory_arguments_slope,
        Self::LookupCoin_cpu_arguments_intercept,
        Self::LookupCoin_cpu_arguments_slope,
        Self::LookupCoin_memory_arguments,
        Self::UnionValue_cpu_arguments_c00,
        Self::UnionValue_cpu_arguments_c10,
        Self::UnionValue_cpu_arguments_c01,
        Self::UnionValue_cpu_arguments_c11,
        Self::UnionValue_memory_arguments_intercept,
        Self::UnionValue_memory_arguments_slope,
        Self::ValueContains_cpu_arguments_constant,
        Self::ValueContains_cpu_arguments_model_arguments_intercept,
        Self::ValueContains_cpu_arguments_model_arguments_slope1,
        Self::ValueContains_cpu_arguments_model_arguments_slope2,
        Self::ValueContains_memory_arguments,
        Self::ValueData_cpu_arguments_intercept,
        Self::ValueData_cpu_arguments_slope,
        Self::ValueData_memory_arguments_intercept,
        Self::ValueData_memory_arguments_slope,
        Self::UnValueData_cpu_arguments_c0,
        Self::UnValueData_cpu_arguments_c1,
        Self::UnValueData_cpu_arguments_c2,
        Self::UnValueData_memory_arguments_intercept,
        Self::UnValueData_memory_arguments_slope,
        Self::ScaleValue_cpu_arguments_intercept,
        Self::ScaleValue_cpu_arguments_slope,
        Self::ScaleValue_memory_arguments_intercept,
        Self::ScaleValue_memory_arguments_slope,
    ];

    /// Order (and which) in which parameters are defined in V3.
    pub const V3: [Self; 350] = [
        Self::AddInteger_cpu_arguments_intercept,
        Self::AddInteger_cpu_arguments_slope,
        Self::AddInteger_memory_arguments_intercept,
        Self::AddInteger_memory_arguments_slope,
        Self::AppendByteString_cpu_arguments_intercept,
        Self::AppendByteString_cpu_arguments_slope,
        Self::AppendByteString_memory_arguments_intercept,
        Self::AppendByteString_memory_arguments_slope,
        Self::AppendString_cpu_arguments_intercept,
        Self::AppendString_cpu_arguments_slope,
        Self::AppendString_memory_arguments_intercept,
        Self::AppendString_memory_arguments_slope,
        Self::BData_cpu_arguments,
        Self::BData_memory_arguments,
        Self::Blake2b_256_cpu_arguments_intercept,
        Self::Blake2b_256_cpu_arguments_slope,
        Self::Blake2b_256_memory_arguments,
        Self::CekApplyCost_exBudgetCPU,
        Self::CekApplyCost_exBudgetMemory,
        Self::CekBuiltinCost_exBudgetCPU,
        Self::CekBuiltinCost_exBudgetMemory,
        Self::CekConstCost_exBudgetCPU,
        Self::CekConstCost_exBudgetMemory,
        Self::CekDelayCost_exBudgetCPU,
        Self::CekDelayCost_exBudgetMemory,
        Self::CekForceCost_exBudgetCPU,
        Self::CekForceCost_exBudgetMemory,
        Self::CekLamCost_exBudgetCPU,
        Self::CekLamCost_exBudgetMemory,
        Self::CekStartupCost_exBudgetCPU,
        Self::CekStartupCost_exBudgetMemory,
        Self::CekVarCost_exBudgetCPU,
        Self::CekVarCost_exBudgetMemory,
        Self::ChooseData_cpu_arguments,
        Self::ChooseData_memory_arguments,
        Self::ChooseList_cpu_arguments,
        Self::ChooseList_memory_arguments,
        Self::ChooseUnit_cpu_arguments,
        Self::ChooseUnit_memory_arguments,
        Self::ConsByteString_cpu_arguments_intercept,
        Self::ConsByteString_cpu_arguments_slope,
        Self::ConsByteString_memory_arguments_intercept,
        Self::ConsByteString_memory_arguments_slope,
        Self::ConstrData_cpu_arguments,
        Self::ConstrData_memory_arguments,
        Self::DecodeUtf8_cpu_arguments_intercept,
        Self::DecodeUtf8_cpu_arguments_slope,
        Self::DecodeUtf8_memory_arguments_intercept,
        Self::DecodeUtf8_memory_arguments_slope,
        Self::DivideInteger_cpu_arguments_constant,
        Self::DivideInteger_cpu_arguments_model_arguments_c00,
        Self::DivideInteger_cpu_arguments_model_arguments_c01,
        Self::DivideInteger_cpu_arguments_model_arguments_c02,
        Self::DivideInteger_cpu_arguments_model_arguments_c10,
        Self::DivideInteger_cpu_arguments_model_arguments_c11,
        Self::DivideInteger_cpu_arguments_model_arguments_c20,
        Self::DivideInteger_cpu_arguments_model_arguments_minimum,
        Self::DivideInteger_memory_arguments_intercept,
        Self::DivideInteger_memory_arguments_minimum,
        Self::DivideInteger_memory_arguments_slope,
        Self::EncodeUtf8_cpu_arguments_intercept,
        Self::EncodeUtf8_cpu_arguments_slope,
        Self::EncodeUtf8_memory_arguments_intercept,
        Self::EncodeUtf8_memory_arguments_slope,
        Self::EqualsByteString_cpu_arguments_constant,
        Self::EqualsByteString_cpu_arguments_intercept,
        Self::EqualsByteString_cpu_arguments_slope,
        Self::EqualsByteString_memory_arguments,
        Self::EqualsData_cpu_arguments_intercept,
        Self::EqualsData_cpu_arguments_slope,
        Self::EqualsData_memory_arguments,
        Self::EqualsInteger_cpu_arguments_intercept,
        Self::EqualsInteger_cpu_arguments_slope,
        Self::EqualsInteger_memory_arguments,
        Self::EqualsString_cpu_arguments_constant,
        Self::EqualsString_cpu_arguments_intercept,
        Self::EqualsString_cpu_arguments_slope,
        Self::EqualsString_memory_arguments,
        Self::FstPair_cpu_arguments,
        Self::FstPair_memory_arguments,
        Self::HeadList_cpu_arguments,
        Self::HeadList_memory_arguments,
        Self::IData_cpu_arguments,
        Self::IData_memory_arguments,
        Self::IfThenElse_cpu_arguments,
        Self::IfThenElse_memory_arguments,
        Self::IndexByteString_cpu_arguments,
        Self::IndexByteString_memory_arguments,
        Self::LengthOfByteString_cpu_arguments,
        Self::LengthOfByteString_memory_arguments,
        Self::LessThanByteString_cpu_arguments_intercept,
        Self::LessThanByteString_cpu_arguments_slope,
        Self::LessThanByteString_memory_arguments,
        Self::LessThanEqualsByteString_cpu_arguments_intercept,
        Self::LessThanEqualsByteString_cpu_arguments_slope,
        Self::LessThanEqualsByteString_memory_arguments,
        Self::LessThanEqualsInteger_cpu_arguments_intercept,
        Self::LessThanEqualsInteger_cpu_arguments_slope,
        Self::LessThanEqualsInteger_memory_arguments,
        Self::LessThanInteger_cpu_arguments_intercept,
        Self::LessThanInteger_cpu_arguments_slope,
        Self::LessThanInteger_memory_arguments,
        Self::ListData_cpu_arguments,
        Self::ListData_memory_arguments,
        Self::MapData_cpu_arguments,
        Self::MapData_memory_arguments,
        Self::MkCons_cpu_arguments,
        Self::MkCons_memory_arguments,
        Self::MkNilData_cpu_arguments,
        Self::MkNilData_memory_arguments,
        Self::MkNilPairData_cpu_arguments,
        Self::MkNilPairData_memory_arguments,
        Self::MkPairData_cpu_arguments,
        Self::MkPairData_memory_arguments,
        Self::ModInteger_cpu_arguments_constant,
        Self::ModInteger_cpu_arguments_model_arguments_c00,
        Self::ModInteger_cpu_arguments_model_arguments_c01,
        Self::ModInteger_cpu_arguments_model_arguments_c02,
        Self::ModInteger_cpu_arguments_model_arguments_c10,
        Self::ModInteger_cpu_arguments_model_arguments_c11,
        Self::ModInteger_cpu_arguments_model_arguments_c20,
        Self::ModInteger_cpu_arguments_model_arguments_minimum,
        Self::ModInteger_memory_arguments_intercept,
        Self::ModInteger_memory_arguments_slope,
        Self::MultiplyInteger_cpu_arguments_intercept,
        Self::MultiplyInteger_cpu_arguments_slope,
        Self::MultiplyInteger_memory_arguments_intercept,
        Self::MultiplyInteger_memory_arguments_slope,
        Self::NullList_cpu_arguments,
        Self::NullList_memory_arguments,
        Self::QuotientInteger_cpu_arguments_constant,
        Self::QuotientInteger_cpu_arguments_model_arguments_c00,
        Self::QuotientInteger_cpu_arguments_model_arguments_c01,
        Self::QuotientInteger_cpu_arguments_model_arguments_c02,
        Self::QuotientInteger_cpu_arguments_model_arguments_c10,
        Self::QuotientInteger_cpu_arguments_model_arguments_c11,
        Self::QuotientInteger_cpu_arguments_model_arguments_c20,
        Self::QuotientInteger_cpu_arguments_model_arguments_minimum,
        Self::QuotientInteger_memory_arguments_intercept,
        Self::QuotientInteger_memory_arguments_minimum,
        Self::QuotientInteger_memory_arguments_slope,
        Self::RemainderInteger_cpu_arguments_constant,
        Self::RemainderInteger_cpu_arguments_model_arguments_c00,
        Self::RemainderInteger_cpu_arguments_model_arguments_c01,
        Self::RemainderInteger_cpu_arguments_model_arguments_c02,
        Self::RemainderInteger_cpu_arguments_model_arguments_c10,
        Self::RemainderInteger_cpu_arguments_model_arguments_c11,
        Self::RemainderInteger_cpu_arguments_model_arguments_c20,
        Self::RemainderInteger_cpu_arguments_model_arguments_minimum,
        Self::RemainderInteger_memory_arguments_intercept,
        Self::RemainderInteger_memory_arguments_slope,
        Self::SerialiseData_cpu_arguments_intercept,
        Self::SerialiseData_cpu_arguments_slope,
        Self::SerialiseData_memory_arguments_intercept,
        Self::SerialiseData_memory_arguments_slope,
        Self::Sha2_256_cpu_arguments_intercept,
        Self::Sha2_256_cpu_arguments_slope,
        Self::Sha2_256_memory_arguments,
        Self::Sha3_256_cpu_arguments_intercept,
        Self::Sha3_256_cpu_arguments_slope,
        Self::Sha3_256_memory_arguments,
        Self::SliceByteString_cpu_arguments_intercept,
        Self::SliceByteString_cpu_arguments_slope,
        Self::SliceByteString_memory_arguments_intercept,
        Self::SliceByteString_memory_arguments_slope,
        Self::SndPair_cpu_arguments,
        Self::SndPair_memory_arguments,
        Self::SubtractInteger_cpu_arguments_intercept,
        Self::SubtractInteger_cpu_arguments_slope,
        Self::SubtractInteger_memory_arguments_intercept,
        Self::SubtractInteger_memory_arguments_slope,
        Self::TailList_cpu_arguments,
        Self::TailList_memory_arguments,
        Self::Trace_cpu_arguments,
        Self::Trace_memory_arguments,
        Self::UnBData_cpu_arguments,
        Self::UnBData_memory_arguments,
        Self::UnConstrData_cpu_arguments,
        Self::UnConstrData_memory_arguments,
        Self::UnIData_cpu_arguments,
        Self::UnIData_memory_arguments,
        Self::UnListData_cpu_arguments,
        Self::UnListData_memory_arguments,
        Self::UnMapData_cpu_arguments,
        Self::UnMapData_memory_arguments,
        Self::VerifyEcdsaSecp256k1Signature_cpu_arguments,
        Self::VerifyEcdsaSecp256k1Signature_memory_arguments,
        Self::VerifyEd25519Signature_cpu_arguments_intercept,
        Self::VerifyEd25519Signature_cpu_arguments_slope,
        Self::VerifyEd25519Signature_memory_arguments,
        Self::VerifySchnorrSecp256k1Signature_cpu_arguments_intercept,
        Self::VerifySchnorrSecp256k1Signature_cpu_arguments_slope,
        Self::VerifySchnorrSecp256k1Signature_memory_arguments,
        Self::CekConstrCost_exBudgetCPU,
        Self::CekConstrCost_exBudgetMemory,
        Self::CekCaseCost_exBudgetCPU,
        Self::CekCaseCost_exBudgetMemory,
        Self::Bls12_381_G1_add_cpu_arguments,
        Self::Bls12_381_G1_add_memory_arguments,
        Self::Bls12_381_G1_compress_cpu_arguments,
        Self::Bls12_381_G1_compress_memory_arguments,
        Self::Bls12_381_G1_equal_cpu_arguments,
        Self::Bls12_381_G1_equal_memory_arguments,
        Self::Bls12_381_G1_hashToGroup_cpu_arguments_intercept,
        Self::Bls12_381_G1_hashToGroup_cpu_arguments_slope,
        Self::Bls12_381_G1_hashToGroup_memory_arguments,
        Self::Bls12_381_G1_neg_cpu_arguments,
        Self::Bls12_381_G1_neg_memory_arguments,
        Self::Bls12_381_G1_scalarMul_cpu_arguments_intercept,
        Self::Bls12_381_G1_scalarMul_cpu_arguments_slope,
        Self::Bls12_381_G1_scalarMul_memory_arguments,
        Self::Bls12_381_G1_uncompress_cpu_arguments,
        Self::Bls12_381_G1_uncompress_memory_arguments,
        Self::Bls12_381_G2_add_cpu_arguments,
        Self::Bls12_381_G2_add_memory_arguments,
        Self::Bls12_381_G2_compress_cpu_arguments,
        Self::Bls12_381_G2_compress_memory_arguments,
        Self::Bls12_381_G2_equal_cpu_arguments,
        Self::Bls12_381_G2_equal_memory_arguments,
        Self::Bls12_381_G2_hashToGroup_cpu_arguments_intercept,
        Self::Bls12_381_G2_hashToGroup_cpu_arguments_slope,
        Self::Bls12_381_G2_hashToGroup_memory_arguments,
        Self::Bls12_381_G2_neg_cpu_arguments,
        Self::Bls12_381_G2_neg_memory_arguments,
        Self::Bls12_381_G2_scalarMul_cpu_arguments_intercept,
        Self::Bls12_381_G2_scalarMul_cpu_arguments_slope,
        Self::Bls12_381_G2_scalarMul_memory_arguments,
        Self::Bls12_381_G2_uncompress_cpu_arguments,
        Self::Bls12_381_G2_uncompress_memory_arguments,
        Self::Bls12_381_finalVerify_cpu_arguments,
        Self::Bls12_381_finalVerify_memory_arguments,
        Self::Bls12_381_millerLoop_cpu_arguments,
        Self::Bls12_381_millerLoop_memory_arguments,
        Self::Bls12_381_mulMlResult_cpu_arguments,
        Self::Bls12_381_mulMlResult_memory_arguments,
        Self::Keccak_256_cpu_arguments_intercept,
        Self::Keccak_256_cpu_arguments_slope,
        Self::Keccak_256_memory_arguments,
        Self::Blake2b_224_cpu_arguments_intercept,
        Self::Blake2b_224_cpu_arguments_slope,
        Self::Blake2b_224_memory_arguments,
        Self::IntegerToByteString_cpu_arguments_c0,
        Self::IntegerToByteString_cpu_arguments_c1,
        Self::IntegerToByteString_cpu_arguments_c2,
        Self::IntegerToByteString_memory_arguments_intercept,
        Self::IntegerToByteString_memory_arguments_slope,
        Self::ByteStringToInteger_cpu_arguments_c0,
        Self::ByteStringToInteger_cpu_arguments_c1,
        Self::ByteStringToInteger_cpu_arguments_c2,
        Self::ByteStringToInteger_memory_arguments_intercept,
        Self::ByteStringToInteger_memory_arguments_slope,
        Self::AndByteString_cpu_arguments_intercept,
        Self::AndByteString_cpu_arguments_slope1,
        Self::AndByteString_cpu_arguments_slope2,
        Self::AndByteString_memory_arguments_intercept,
        Self::AndByteString_memory_arguments_slope,
        Self::OrByteString_cpu_arguments_intercept,
        Self::OrByteString_cpu_arguments_slope1,
        Self::OrByteString_cpu_arguments_slope2,
        Self::OrByteString_memory_arguments_intercept,
        Self::OrByteString_memory_arguments_slope,
        Self::XorByteString_cpu_arguments_intercept,
        Self::XorByteString_cpu_arguments_slope1,
        Self::XorByteString_cpu_arguments_slope2,
        Self::XorByteString_memory_arguments_intercept,
        Self::XorByteString_memory_arguments_slope,
        Self::ComplementByteString_cpu_arguments_intercept,
        Self::ComplementByteString_cpu_arguments_slope,
        Self::ComplementByteString_memory_arguments_intercept,
        Self::ComplementByteString_memory_arguments_slope,
        Self::ReadBit_cpu_arguments,
        Self::ReadBit_memory_arguments,
        Self::WriteBits_cpu_arguments_intercept,
        Self::WriteBits_cpu_arguments_slope,
        Self::WriteBits_memory_arguments_intercept,
        Self::WriteBits_memory_arguments_slope,
        Self::ReplicateByte_cpu_arguments_intercept,
        Self::ReplicateByte_cpu_arguments_slope,
        Self::ReplicateByte_memory_arguments_intercept,
        Self::ReplicateByte_memory_arguments_slope,
        Self::ShiftByteString_cpu_arguments_intercept,
        Self::ShiftByteString_cpu_arguments_slope,
        Self::ShiftByteString_memory_arguments_intercept,
        Self::ShiftByteString_memory_arguments_slope,
        Self::RotateByteString_cpu_arguments_intercept,
        Self::RotateByteString_cpu_arguments_slope,
        Self::RotateByteString_memory_arguments_intercept,
        Self::RotateByteString_memory_arguments_slope,
        Self::CountSetBits_cpu_arguments_intercept,
        Self::CountSetBits_cpu_arguments_slope,
        Self::CountSetBits_memory_arguments,
        Self::FindFirstSetBit_cpu_arguments_intercept,
        Self::FindFirstSetBit_cpu_arguments_slope,
        Self::FindFirstSetBit_memory_arguments,
        Self::Ripemd_160_cpu_arguments_intercept,
        Self::Ripemd_160_cpu_arguments_slope,
        Self::Ripemd_160_memory_arguments,
        Self::ExpModInteger_cpu_arguments_coefficient00,
        Self::ExpModInteger_cpu_arguments_coefficient11,
        Self::ExpModInteger_cpu_arguments_coefficient12,
        Self::ExpModInteger_memory_arguments_intercept,
        Self::ExpModInteger_memory_arguments_slope,
        Self::DropList_cpu_arguments_intercept,
        Self::DropList_cpu_arguments_slope,
        Self::DropList_memory_arguments,
        Self::LengthOfArray_cpu_arguments,
        Self::LengthOfArray_memory_arguments,
        Self::ListToArray_cpu_arguments_intercept,
        Self::ListToArray_cpu_arguments_slope,
        Self::ListToArray_memory_arguments_intercept,
        Self::ListToArray_memory_arguments_slope,
        Self::IndexArray_cpu_arguments,
        Self::IndexArray_memory_arguments,
        Self::Bls12_381_G1_multiScalarMul_cpu_arguments_intercept,
        Self::Bls12_381_G1_multiScalarMul_cpu_arguments_slope,
        Self::Bls12_381_G1_multiScalarMul_memory_arguments,
        Self::Bls12_381_G2_multiScalarMul_cpu_arguments_intercept,
        Self::Bls12_381_G2_multiScalarMul_cpu_arguments_slope,
        Self::Bls12_381_G2_multiScalarMul_memory_arguments,
        Self::InsertCoin_cpu_arguments_intercept,
        Self::InsertCoin_cpu_arguments_slope,
        Self::InsertCoin_memory_arguments_intercept,
        Self::InsertCoin_memory_arguments_slope,
        Self::LookupCoin_cpu_arguments_intercept,
        Self::LookupCoin_cpu_arguments_slope,
        Self::LookupCoin_memory_arguments,
        Self::UnionValue_cpu_arguments_c00,
        Self::UnionValue_cpu_arguments_c10,
        Self::UnionValue_cpu_arguments_c01,
        Self::UnionValue_cpu_arguments_c11,
        Self::UnionValue_memory_arguments_intercept,
        Self::UnionValue_memory_arguments_slope,
        Self::ValueContains_cpu_arguments_constant,
        Self::ValueContains_cpu_arguments_model_arguments_intercept,
        Self::ValueContains_cpu_arguments_model_arguments_slope1,
        Self::ValueContains_cpu_arguments_model_arguments_slope2,
        Self::ValueContains_memory_arguments,
        Self::ValueData_cpu_arguments_intercept,
        Self::ValueData_cpu_arguments_slope,
        Self::ValueData_memory_arguments_intercept,
        Self::ValueData_memory_arguments_slope,
        Self::UnValueData_cpu_arguments_c0,
        Self::UnValueData_cpu_arguments_c1,
        Self::UnValueData_cpu_arguments_c2,
        Self::UnValueData_memory_arguments_intercept,
        Self::UnValueData_memory_arguments_slope,
        Self::ScaleValue_cpu_arguments_intercept,
        Self::ScaleValue_cpu_arguments_slope,
        Self::ScaleValue_memory_arguments_intercept,
        Self::ScaleValue_memory_arguments_slope,
    ];
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
    pub bls12_381_g1_add: CostingFun<TwoArguments>,
    pub bls12_381_g1_neg: CostingFun<OneArgument>,
    pub bls12_381_g1_scalar_mul: CostingFun<TwoArguments>,
    pub bls12_381_g1_equal: CostingFun<TwoArguments>,
    pub bls12_381_g1_compress: CostingFun<OneArgument>,
    pub bls12_381_g1_uncompress: CostingFun<OneArgument>,
    pub bls12_381_g1_hash_to_group: CostingFun<TwoArguments>,
    pub bls12_381_g2_add: CostingFun<TwoArguments>,
    pub bls12_381_g2_neg: CostingFun<OneArgument>,
    pub bls12_381_g2_scalar_mul: CostingFun<TwoArguments>,
    pub bls12_381_g2_equal: CostingFun<TwoArguments>,
    pub bls12_381_g2_compress: CostingFun<OneArgument>,
    pub bls12_381_g2_uncompress: CostingFun<OneArgument>,
    pub bls12_381_g2_hash_to_group: CostingFun<TwoArguments>,
    pub bls12_381_miller_loop: CostingFun<TwoArguments>,
    pub bls12_381_mul_ml_result: CostingFun<TwoArguments>,
    pub bls12_381_final_verify: CostingFun<TwoArguments>,
    // bitwise
    pub integer_to_byte_string: CostingFun<ThreeArguments>,
    pub byte_string_to_integer: CostingFun<TwoArguments>,
    pub and_byte_string: CostingFun<ThreeArguments>,
    pub or_byte_string: CostingFun<ThreeArguments>,
    pub xor_byte_string: CostingFun<ThreeArguments>,
    pub complement_byte_string: CostingFun<OneArgument>,
    pub read_bit: CostingFun<TwoArguments>,
    pub write_bits: CostingFun<ThreeArguments>,
    pub replicate_byte: CostingFun<TwoArguments>,
    pub shift_byte_string: CostingFun<TwoArguments>,
    pub rotate_byte_string: CostingFun<TwoArguments>,
    pub count_set_bits: CostingFun<OneArgument>,
    pub find_first_set_bit: CostingFun<OneArgument>,
    pub ripemd_160: CostingFun<OneArgument>,
    pub exp_mod_int: CostingFun<ThreeArguments>,
    pub drop_list: CostingFun<TwoArguments>,
    pub length_of_array: CostingFun<OneArgument>,
    pub list_to_array: CostingFun<OneArgument>,
    pub index_array: CostingFun<TwoArguments>,
    pub bls12_381_g1_multi_scalar_mul: CostingFun<TwoArguments>,
    pub bls12_381_g2_multi_scalar_mul: CostingFun<TwoArguments>,
    pub insert_coin: CostingFun<FourArguments>,
    pub lookup_coin: CostingFun<ThreeArguments>,
    pub union_value: CostingFun<TwoArguments>,
    pub value_contains: CostingFun<TwoArguments>,
    pub value_data: CostingFun<OneArgument>,
    pub un_value_data: CostingFun<OneArgument>,
    pub scale_value: CostingFun<TwoArguments>,
}

impl BuiltinCosts {
    pub const DEFAULT_V1: [i64; 332] = [
        100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356, 4, 16000,
        100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100, 16000, 100,
        94375, 32, 132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189, 769, 4, 2, 85848,
        228465, 122, 0, 1, 1, 1000, 42921, 4, 2, 30623, 28755, 75, 1, 898148, 27279, 1, 51775, 558,
        1, 39184, 1000, 60594, 1, 141895, 32, 83150, 32, 15299, 32, 76049, 1, 13169, 4, 22100, 10,
        28999, 74, 1, 28999, 74, 1, 43285, 552, 1, 44749, 541, 1, 33852, 32, 68246, 32, 72362, 32,
        7243, 32, 7391, 32, 11546, 32, 85848, 228465, 122, 0, 1, 1, 90434, 519, 0, 1, 74433, 32,
        85848, 228465, 122, 0, 1, 1, 85848, 228465, 122, 0, 1, 1, 270652, 22588, 4, 1457325, 64566,
        4, 20467, 1, 4, 0, 141992, 32, 100788, 420, 1, 1, 81663, 32, 59498, 32, 20142, 32, 24588,
        32, 20744, 32, 25933, 32, 24623, 32, 53384111, 14333, 10, 955506, 213312, 0, 2, 43053543,
        10, 43574283, 26308, 10, 16000, 100, 16000, 100, 962335, 18, 2780678, 6, 442008, 1,
        52538055, 3756, 18, 267929, 18, 76433006, 8868, 18, 52948122, 18, 1995836, 36, 3227919, 12,
        901022, 1, 166917843, 4307, 36, 284546, 36, 158221314, 26549, 36, 74698472, 36, 333849714,
        1, 254006273, 72, 2174038, 72, 2261318, 64571, 4, 207616, 8310, 4, 1293828, 28716, 63, 0,
        1, 1006041, 43623, 251, 0, 1, 100181, 726, 719, 0, 1, 100181, 726, 719, 0, 1, 100181, 726,
        719, 0, 1, 107878, 680, 0, 1, 95336, 1, 281145, 18848, 0, 1, 180194, 159, 1, 1, 158519,
        8942, 0, 1, 159378, 8813, 0, 1, 107490, 3298, 1, 106057, 655, 1, 1964219, 24520, 3, 607153,
        231697, 53144, 0, 1, 116711, 1957, 4, 231883, 10, 1000, 24838, 7, 1, 232010, 32, 321837444,
        25087669, 18, 617887431, 67302824, 36, 356924, 18413, 45, 21, 219951, 9444, 1, 1000,
        172116, 183150, 6, 24, 21, 213283, 618401, 1998, 28258, 1, 1000, 38159, 2, 22, 1000, 95933,
        1, 1, 11, 1000, 277577, 12, 21,
    ];

    pub fn v1() -> Self {
        initialize_cost_model(&Language::PlutusV1, &Self::DEFAULT_V1[..]).builtin_costs
    }

    const DEFAULT_V2: [i64; 332] = [
        100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356, 4, 16000,
        100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100, 16000, 100,
        94375, 32, 132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189, 769, 4, 2, 85848,
        228465, 122, 0, 1, 1, 1000, 42921, 4, 2, 30623, 28755, 75, 1, 898148, 27279, 1, 51775, 558,
        1, 39184, 1000, 60594, 1, 141895, 32, 83150, 32, 15299, 32, 76049, 1, 13169, 4, 22100, 10,
        28999, 74, 1, 28999, 74, 1, 43285, 552, 1, 44749, 541, 1, 33852, 32, 68246, 32, 72362, 32,
        7243, 32, 7391, 32, 11546, 32, 85848, 228465, 122, 0, 1, 1, 90434, 519, 0, 1, 74433, 32,
        85848, 228465, 122, 0, 1, 1, 85848, 228465, 122, 0, 1, 1, 955506, 213312, 0, 2, 270652,
        22588, 4, 1457325, 64566, 4, 20467, 1, 4, 0, 141992, 32, 100788, 420, 1, 1, 81663, 32,
        59498, 32, 20142, 32, 24588, 32, 20744, 32, 25933, 32, 24623, 32, 43053543, 10, 53384111,
        14333, 10, 43574283, 26308, 10, 1293828, 28716, 63, 0, 1, 1006041, 43623, 251, 0, 1, 16000,
        100, 16000, 100, 962335, 18, 2780678, 6, 442008, 1, 52538055, 3756, 18, 267929, 18,
        76433006, 8868, 18, 52948122, 18, 1995836, 36, 3227919, 12, 901022, 1, 166917843, 4307, 36,
        284546, 36, 158221314, 26549, 36, 74698472, 36, 333849714, 1, 254006273, 72, 2174038, 72,
        2261318, 64571, 4, 207616, 8310, 4, 100181, 726, 719, 0, 1, 100181, 726, 719, 0, 1, 100181,
        726, 719, 0, 1, 107878, 680, 0, 1, 95336, 1, 281145, 18848, 0, 1, 180194, 159, 1, 1,
        158519, 8942, 0, 1, 159378, 8813, 0, 1, 107490, 3298, 1, 106057, 655, 1, 1964219, 24520, 3,
        607153, 231697, 53144, 0, 1, 116711, 1957, 4, 231883, 10, 1000, 24838, 7, 1, 232010, 32,
        321837444, 25087669, 18, 617887431, 67302824, 36, 356924, 18413, 45, 21, 219951, 9444, 1,
        1000, 172116, 183150, 6, 24, 21, 213283, 618401, 1998, 28258, 1, 1000, 38159, 2, 22, 1000,
        95933, 1, 1, 11, 1000, 277577, 12, 21,
    ];

    pub fn v2() -> Self {
        initialize_cost_model(&Language::PlutusV2, &Self::DEFAULT_V2[..]).builtin_costs
    }

    const DEFAULT_V3: [i64; 350] = [
        100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356, 4, 16000,
        100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100, 16000, 100,
        94375, 32, 132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189, 769, 4, 2, 85848,
        123203, 7305, -900, 1716, 960, 57, 85848, 0, 1, 1, 1000, 42921, 4, 2, 30623, 28755, 75, 1,
        898148, 27279, 1, 51775, 558, 1, 39184, 1000, 60594, 1, 141895, 32, 83150, 32, 15299, 32,
        76049, 1, 13169, 4, 22100, 10, 28999, 74, 1, 28999, 74, 1, 43285, 552, 1, 44749, 541, 1,
        33852, 32, 68246, 32, 72362, 32, 7243, 32, 7391, 32, 11546, 32, 85848, 123203, 7305, -900,
        1716, 960, 57, 85848, 0, 1, 90434, 519, 0, 1, 74433, 32, 85848, 123203, 7305, -900, 1716,
        960, 57, 85848, 0, 1, 1, 85848, 123203, 7305, -900, 1716, 960, 57, 85848, 0, 1, 955506,
        213312, 0, 2, 270652, 22588, 4, 1457325, 64566, 4, 20467, 1, 4, 0, 141992, 32, 100788, 420,
        1, 1, 81663, 32, 59498, 32, 20142, 32, 24588, 32, 20744, 32, 25933, 32, 24623, 32,
        43053543, 10, 53384111, 14333, 10, 43574283, 26308, 10, 16000, 100, 16000, 100, 962335, 18,
        2780678, 6, 442008, 1, 52538055, 3756, 18, 267929, 18, 76433006, 8868, 18, 52948122, 18,
        1995836, 36, 3227919, 12, 901022, 1, 166917843, 4307, 36, 284546, 36, 158221314, 26549, 36,
        74698472, 36, 333849714, 1, 254006273, 72, 2174038, 72, 2261318, 64571, 4, 207616, 8310, 4,
        1293828, 28716, 63, 0, 1, 1006041, 43623, 251, 0, 1, 100181, 726, 719, 0, 1, 100181, 726,
        719, 0, 1, 100181, 726, 719, 0, 1, 107878, 680, 0, 1, 95336, 1, 281145, 18848, 0, 1,
        180194, 159, 1, 1, 158519, 8942, 0, 1, 159378, 8813, 0, 1, 107490, 3298, 1, 106057, 655, 1,
        1964219, 24520, 3, 607153, 231697, 53144, 0, 1, 116711, 1957, 4, 231883, 10, 1000, 24838,
        7, 1, 232010, 32, 321837444, 25087669, 18, 617887431, 67302824, 36, 356924, 18413, 45, 21,
        219951, 9444, 1, 1000, 172116, 183150, 6, 24, 21, 213283, 618401, 1998, 28258, 1, 1000,
        38159, 2, 22, 1000, 95933, 1, 1, 11, 1000, 277577, 12, 21,
    ];

    pub fn v3() -> Self {
        initialize_cost_model(&Language::PlutusV3, &Self::DEFAULT_V3[..]).builtin_costs
    }
}

impl Default for BuiltinCosts {
    fn default() -> Self {
        BuiltinCosts::v3()
    }
}

fn list_len_or_value_ex_mem(arg: &Value) -> i64 {
    match arg.unwrap_list() {
        Ok((_, items)) => items.len() as i64,
        Err(_) => arg.to_ex_mem(),
    }
}

fn literal_abs_as_i64_saturating(literal: &BigInt) -> i64 {
    let abs = literal.abs();

    if abs > BigInt::from(i64::MAX) {
        i64::MAX
    } else {
        abs.try_into().expect("literal bounded to i64::MAX")
    }
}

fn value_total_size(arg: &Value) -> Result<i64, Error> {
    Ok(arg.unwrap_value()?.total_size() as i64)
}

fn value_max_depth(arg: &Value) -> Result<i64, Error> {
    let value = arg.unwrap_value()?;
    let log2_plus_1 = |size: usize| {
        if size == 0 {
            0
        } else {
            (usize::BITS - size.leading_zeros()) as i64
        }
    };

    Ok(log2_plus_1(value.outer_size()) + log2_plus_1(value.max_inner_size()))
}

impl BuiltinCosts {
    pub fn to_ex_budget(
        &self,
        fun: DefaultFunction,
        args: &[Value],
        semantics: BuiltinSemantics,
    ) -> Result<ExBudget, Error> {
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
                mem: self.append_string.mem.cost(
                    args[0].to_ex_mem_with_semantics(semantics),
                    args[1].to_ex_mem_with_semantics(semantics),
                ),
                cpu: self.append_string.cpu.cost(
                    args[0].to_ex_mem_with_semantics(semantics),
                    args[1].to_ex_mem_with_semantics(semantics),
                ),
            },
            DefaultFunction::EqualsString => ExBudget {
                mem: self.equals_string.mem.cost(
                    args[0].to_ex_mem_with_semantics(semantics),
                    args[1].to_ex_mem_with_semantics(semantics),
                ),
                cpu: self.equals_string.cpu.cost(
                    args[0].to_ex_mem_with_semantics(semantics),
                    args[1].to_ex_mem_with_semantics(semantics),
                ),
            },
            DefaultFunction::EncodeUtf8 => ExBudget {
                mem: self
                    .encode_utf8
                    .mem
                    .cost(args[0].to_ex_mem_with_semantics(semantics)),
                cpu: self
                    .encode_utf8
                    .cpu
                    .cost(args[0].to_ex_mem_with_semantics(semantics)),
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
            DefaultFunction::Bls12_381_G1_MultiScalarMul => {
                // The cost is linear in the *length* of the first list argument
                // (number of scalars), matching Plutus' ExMemoryUsage for lists.
                // For malformed inputs, fall back to the value's generic memory
                // usage so we don't silently under-cost the error path.
                let scalars_len = list_len_or_value_ex_mem(&args[0]);

                ExBudget {
                    mem: self
                        .bls12_381_g1_multi_scalar_mul
                        .mem
                        .cost(scalars_len, args[1].to_ex_mem()),
                    cpu: self
                        .bls12_381_g1_multi_scalar_mul
                        .cpu
                        .cost(scalars_len, args[1].to_ex_mem()),
                }
            }
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
            DefaultFunction::Bls12_381_G2_MultiScalarMul => {
                // The cost model measures the scalar list (first argument) by
                // its length (number of elements), not by its summed memory.
                // For malformed inputs, fall back to generic value memory usage
                // instead of silently charging as if the list were empty.
                let scalars_len = list_len_or_value_ex_mem(&args[0]);
                let points_len = list_len_or_value_ex_mem(&args[1]);

                ExBudget {
                    mem: self
                        .bls12_381_g2_multi_scalar_mul
                        .mem
                        .cost(scalars_len, points_len),
                    cpu: self
                        .bls12_381_g2_multi_scalar_mul
                        .cpu
                        .cost(scalars_len, points_len),
                }
            }
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
                let arg1 = literal_abs_as_i64_saturating(literal);

                ExBudget {
                    mem: self.shift_byte_string.mem.cost(args[0].to_ex_mem(), arg1),
                    cpu: self.shift_byte_string.cpu.cost(args[0].to_ex_mem(), arg1),
                }
            }
            DefaultFunction::RotateByteString => {
                let literal = args[1].unwrap_integer()?;
                let arg1 = literal_abs_as_i64_saturating(literal);

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
            DefaultFunction::DropList => {
                // `dropList`'s first argument is costed *literally*: the size fed
                // to the cost function is `|n|` (the absolute literal value),
                // saturated to `i64::MAX`, rather than the integer's word size.
                let literal = args[0].unwrap_integer()?;
                let arg0 = literal_abs_as_i64_saturating(literal);
                let arg1 = args[1].to_ex_mem();

                ExBudget {
                    mem: self.drop_list.mem.cost(arg0, arg1),
                    cpu: self.drop_list.cpu.cost(arg0, arg1),
                }
            }
            DefaultFunction::ExpModInteger => {
                let modulus = args[2].unwrap_integer()?;
                if modulus <= &0.into() || integer_log2(modulus.clone()) >= 8191 {
                    return Err(Error::OutsideNaturalBounds(modulus.clone()));
                }

                let x = args[0].to_ex_mem();
                let y = args[1].to_ex_mem();
                let z = args[2].to_ex_mem();

                ExBudget {
                    mem: self.exp_mod_int.mem.cost(x, y, z),
                    cpu: self.exp_mod_int.cpu.cost(x, y, z),
                }
            }
            DefaultFunction::InsertCoin => {
                let value_size = value_max_depth(&args[3])?;

                ExBudget {
                    mem: self.insert_coin.mem.cost(
                        args[0].to_ex_mem(),
                        args[1].to_ex_mem(),
                        args[2].to_ex_mem(),
                        value_size,
                    ),
                    cpu: self.insert_coin.cpu.cost(
                        args[0].to_ex_mem(),
                        args[1].to_ex_mem(),
                        args[2].to_ex_mem(),
                        value_size,
                    ),
                }
            }
            DefaultFunction::LookupCoin => {
                let value_size = value_max_depth(&args[2])?;

                ExBudget {
                    mem: self.lookup_coin.mem.cost(
                        args[0].to_ex_mem(),
                        args[1].to_ex_mem(),
                        value_size,
                    ),
                    cpu: self.lookup_coin.cpu.cost(
                        args[0].to_ex_mem(),
                        args[1].to_ex_mem(),
                        value_size,
                    ),
                }
            }
            DefaultFunction::UnionValue => {
                let left_size = value_total_size(&args[0])?;
                let right_size = value_total_size(&args[1])?;

                ExBudget {
                    mem: self.union_value.mem.cost(left_size, right_size),
                    cpu: self.union_value.cpu.cost(left_size, right_size),
                }
            }
            DefaultFunction::ValueContains => {
                let left_size = value_total_size(&args[0])?;
                let right_size = value_total_size(&args[1])?;

                ExBudget {
                    mem: self.value_contains.mem.cost(left_size, right_size),
                    cpu: self.value_contains.cpu.cost(left_size, right_size),
                }
            }
            DefaultFunction::ValueData => {
                let value_size = value_total_size(&args[0])?;

                ExBudget {
                    mem: self.value_data.mem.cost(value_size),
                    cpu: self.value_data.cpu.cost(value_size),
                }
            }
            DefaultFunction::UnValueData => {
                let data_size = args[0].data_node_count()?;

                ExBudget {
                    mem: self.un_value_data.mem.cost(data_size),
                    cpu: self.un_value_data.cpu.cost(data_size),
                }
            }
            DefaultFunction::ScaleValue => {
                let value_size = value_total_size(&args[1])?;

                ExBudget {
                    mem: self.scale_value.mem.cost(args[0].to_ex_mem(), value_size),
                    cpu: self.scale_value.cpu.cost(args[0].to_ex_mem(), value_size),
                }
            }
        })
    }
}

pub fn initialize_cost_model(language: &Language, costs: &[i64]) -> CostModel {
    initialize_cost_model_with_semantics(language, BuiltinSemantics::for_language(language), costs)
}

pub fn initialize_cost_model_with_protocol(
    language: &Language,
    protocol_major_version: u16,
    costs: &[i64],
) -> CostModel {
    initialize_cost_model_with_semantics(
        language,
        BuiltinSemantics::for_language_and_protocol(language, protocol_major_version),
        costs,
    )
}

fn initialize_cost_model_with_semantics(
    language: &Language,
    semantics: BuiltinSemantics,
    costs: &[i64],
) -> CostModel {
    use ParamName::*;

    let cost_map: HashMap<ParamName, i64> = match language {
        Language::PlutusV1 => ParamName::V1
            .into_iter()
            .zip(costs.iter().copied())
            .collect(),
        Language::PlutusV2 => ParamName::V2
            .into_iter()
            .zip(costs.iter().copied())
            .collect(),
        Language::PlutusV3 => ParamName::V3
            .into_iter()
            .zip(costs.iter().copied())
            .collect(),
    };

    let get = |param: ParamName| {
        cost_map
            .get(&param)
            .copied()
            .unwrap_or(UNAVAILABLE_BUILTIN_COST_PLACEHOLDER)
    };

    CostModel {
        machine_costs: MachineCosts {
            startup: ExBudget {
                mem: get(CekStartupCost_exBudgetMemory),
                cpu: get(CekStartupCost_exBudgetCPU),
            },
            var: ExBudget {
                mem: get(CekVarCost_exBudgetMemory),
                cpu: get(CekVarCost_exBudgetCPU),
            },
            constant: ExBudget {
                mem: get(CekConstCost_exBudgetMemory),
                cpu: get(CekConstCost_exBudgetCPU),
            },
            lambda: ExBudget {
                mem: get(CekLamCost_exBudgetMemory),
                cpu: get(CekLamCost_exBudgetCPU),
            },
            delay: ExBudget {
                mem: get(CekDelayCost_exBudgetMemory),
                cpu: get(CekDelayCost_exBudgetCPU),
            },
            force: ExBudget {
                mem: get(CekForceCost_exBudgetMemory),
                cpu: get(CekForceCost_exBudgetCPU),
            },
            apply: ExBudget {
                mem: get(CekApplyCost_exBudgetMemory),
                cpu: get(CekApplyCost_exBudgetCPU),
            },
            builtin: ExBudget {
                mem: get(CekBuiltinCost_exBudgetMemory),
                cpu: get(CekBuiltinCost_exBudgetCPU),
            },
            constr: ExBudget {
                mem: get(CekConstrCost_exBudgetMemory),
                cpu: get(CekConstrCost_exBudgetCPU),
            },
            case: ExBudget {
                mem: get(CekCaseCost_exBudgetMemory),
                cpu: get(CekCaseCost_exBudgetCPU),
            },
        },
        builtin_costs: BuiltinCosts {
            add_integer: CostingFun {
                mem: TwoArguments::MaxSize(MaxSize {
                    intercept: get(AddInteger_memory_arguments_intercept),
                    slope: get(AddInteger_memory_arguments_slope),
                }),
                cpu: TwoArguments::MaxSize(MaxSize {
                    intercept: get(AddInteger_cpu_arguments_intercept),
                    slope: get(AddInteger_cpu_arguments_slope),
                }),
            },
            append_byte_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: get(AppendByteString_memory_arguments_intercept),
                    slope: get(AppendByteString_memory_arguments_slope),
                }),
                cpu: TwoArguments::AddedSizes(AddedSizes {
                    intercept: get(AppendByteString_cpu_arguments_intercept),
                    slope: get(AppendByteString_cpu_arguments_slope),
                }),
            },
            append_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: get(AppendString_memory_arguments_intercept),
                    slope: get(AppendString_memory_arguments_slope),
                }),
                cpu: TwoArguments::AddedSizes(AddedSizes {
                    intercept: get(AppendString_cpu_arguments_intercept),
                    slope: get(AppendString_cpu_arguments_slope),
                }),
            },
            b_data: CostingFun {
                mem: OneArgument::ConstantCost(get(BData_memory_arguments)),
                cpu: OneArgument::ConstantCost(get(BData_cpu_arguments)),
            },
            blake2b_256: CostingFun {
                mem: OneArgument::ConstantCost(get(Blake2b_256_memory_arguments)),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: get(Blake2b_256_cpu_arguments_intercept),
                    slope: get(Blake2b_256_cpu_arguments_slope),
                }),
            },
            choose_data: CostingFun {
                mem: SixArguments::ConstantCost(get(ChooseData_memory_arguments)),
                cpu: SixArguments::ConstantCost(get(ChooseData_cpu_arguments)),
            },
            choose_list: CostingFun {
                mem: ThreeArguments::ConstantCost(get(ChooseList_memory_arguments)),
                cpu: ThreeArguments::ConstantCost(get(ChooseList_cpu_arguments)),
            },
            choose_unit: CostingFun {
                mem: TwoArguments::ConstantCost(get(ChooseUnit_memory_arguments)),
                cpu: TwoArguments::ConstantCost(get(ChooseUnit_cpu_arguments)),
            },
            cons_byte_string: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: get(ConsByteString_memory_arguments_intercept),
                    slope: get(ConsByteString_memory_arguments_slope),
                }),
                cpu: TwoArguments::LinearInY(LinearSize {
                    intercept: get(ConsByteString_cpu_arguments_intercept),
                    slope: get(ConsByteString_cpu_arguments_slope),
                }),
            },
            constr_data: CostingFun {
                mem: TwoArguments::ConstantCost(get(ConstrData_memory_arguments)),
                cpu: TwoArguments::ConstantCost(get(ConstrData_cpu_arguments)),
            },
            decode_utf8: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: get(DecodeUtf8_memory_arguments_intercept),
                    slope: get(DecodeUtf8_memory_arguments_slope),
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: get(DecodeUtf8_cpu_arguments_intercept),
                    slope: get(DecodeUtf8_cpu_arguments_slope),
                }),
            },
            divide_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: get(DivideInteger_memory_arguments_intercept),
                    slope: get(DivideInteger_memory_arguments_slope),
                    minimum: get(DivideInteger_memory_arguments_minimum),
                }),
                cpu: match semantics {
                    BuiltinSemantics::A | BuiltinSemantics::B => {
                        TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                            constant: get(DivideInteger_cpu_arguments_constant),
                            model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                                intercept: get(
                                    DivideInteger_cpu_arguments_model_arguments_intercept,
                                ),
                                slope: get(DivideInteger_cpu_arguments_model_arguments_slope),
                            })),
                        })
                    }
                    BuiltinSemantics::D => {
                        TwoArguments::AboveAndBelowDiagonal(ConstantOrTwoArguments {
                            constant: get(DivideInteger_cpu_arguments_constant),
                            model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                                intercept: get(
                                    DivideInteger_cpu_arguments_model_arguments_intercept,
                                ),
                                slope: get(DivideInteger_cpu_arguments_model_arguments_slope),
                            })),
                        })
                    }
                    BuiltinSemantics::C => {
                        TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                            constant: get(DivideInteger_cpu_arguments_constant),
                            model: Box::new(TwoArguments::QuadraticInXAndY(
                                TwoArgumentsQuadraticFunction {
                                    minimum: get(
                                        DivideInteger_cpu_arguments_model_arguments_minimum,
                                    ),
                                    coeff_00: get(DivideInteger_cpu_arguments_model_arguments_c00),
                                    coeff_10: get(DivideInteger_cpu_arguments_model_arguments_c10),
                                    coeff_01: get(DivideInteger_cpu_arguments_model_arguments_c01),
                                    coeff_20: get(DivideInteger_cpu_arguments_model_arguments_c20),
                                    coeff_11: get(DivideInteger_cpu_arguments_model_arguments_c11),
                                    coeff_02: get(DivideInteger_cpu_arguments_model_arguments_c02),
                                },
                            )),
                        })
                    }
                    BuiltinSemantics::E => {
                        TwoArguments::AboveAndBelowDiagonal(ConstantOrTwoArguments {
                            constant: get(DivideInteger_cpu_arguments_constant),
                            model: Box::new(TwoArguments::QuadraticInXAndY(
                                TwoArgumentsQuadraticFunction {
                                    minimum: get(
                                        DivideInteger_cpu_arguments_model_arguments_minimum,
                                    ),
                                    coeff_00: get(DivideInteger_cpu_arguments_model_arguments_c00),
                                    coeff_10: get(DivideInteger_cpu_arguments_model_arguments_c10),
                                    coeff_01: get(DivideInteger_cpu_arguments_model_arguments_c01),
                                    coeff_20: get(DivideInteger_cpu_arguments_model_arguments_c20),
                                    coeff_11: get(DivideInteger_cpu_arguments_model_arguments_c11),
                                    coeff_02: get(DivideInteger_cpu_arguments_model_arguments_c02),
                                },
                            )),
                        })
                    }
                },
            },
            encode_utf8: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: get(EncodeUtf8_memory_arguments_intercept),
                    slope: get(EncodeUtf8_memory_arguments_slope),
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: get(EncodeUtf8_cpu_arguments_intercept),
                    slope: get(EncodeUtf8_cpu_arguments_slope),
                }),
            },
            equals_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(get(EqualsByteString_memory_arguments)),
                cpu: TwoArguments::LinearOnDiagonal(ConstantOrLinear {
                    constant: get(EqualsByteString_cpu_arguments_constant),
                    intercept: get(EqualsByteString_cpu_arguments_intercept),
                    slope: get(EqualsByteString_cpu_arguments_slope),
                }),
            },
            equals_data: CostingFun {
                mem: TwoArguments::ConstantCost(get(EqualsData_memory_arguments)),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: get(EqualsData_cpu_arguments_intercept),
                    slope: get(EqualsData_cpu_arguments_slope),
                }),
            },
            equals_integer: CostingFun {
                mem: TwoArguments::ConstantCost(get(EqualsInteger_memory_arguments)),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: get(EqualsInteger_cpu_arguments_intercept),
                    slope: get(EqualsInteger_cpu_arguments_slope),
                }),
            },
            equals_string: CostingFun {
                mem: TwoArguments::ConstantCost(get(EqualsString_memory_arguments)),
                cpu: TwoArguments::LinearOnDiagonal(ConstantOrLinear {
                    constant: get(EqualsString_cpu_arguments_constant),
                    intercept: get(EqualsString_cpu_arguments_intercept),
                    slope: get(EqualsString_cpu_arguments_slope),
                }),
            },
            fst_pair: CostingFun {
                mem: OneArgument::ConstantCost(get(FstPair_memory_arguments)),
                cpu: OneArgument::ConstantCost(get(FstPair_cpu_arguments)),
            },
            head_list: CostingFun {
                mem: OneArgument::ConstantCost(get(HeadList_memory_arguments)),
                cpu: OneArgument::ConstantCost(get(HeadList_cpu_arguments)),
            },
            i_data: CostingFun {
                mem: OneArgument::ConstantCost(get(IData_memory_arguments)),
                cpu: OneArgument::ConstantCost(get(IData_cpu_arguments)),
            },
            if_then_else: CostingFun {
                mem: ThreeArguments::ConstantCost(get(IfThenElse_memory_arguments)),
                cpu: ThreeArguments::ConstantCost(get(IfThenElse_cpu_arguments)),
            },
            index_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(get(IndexByteString_memory_arguments)),
                cpu: TwoArguments::ConstantCost(get(IndexByteString_cpu_arguments)),
            },
            length_of_byte_string: CostingFun {
                mem: OneArgument::ConstantCost(get(LengthOfByteString_memory_arguments)),
                cpu: OneArgument::ConstantCost(get(LengthOfByteString_cpu_arguments)),
            },
            less_than_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(get(LessThanByteString_memory_arguments)),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: get(LessThanByteString_cpu_arguments_intercept),
                    slope: get(LessThanByteString_cpu_arguments_slope),
                }),
            },
            less_than_equals_byte_string: CostingFun {
                mem: TwoArguments::ConstantCost(get(LessThanEqualsByteString_memory_arguments)),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: get(LessThanEqualsByteString_cpu_arguments_intercept),
                    slope: get(LessThanEqualsByteString_cpu_arguments_slope),
                }),
            },
            less_than_equals_integer: CostingFun {
                mem: TwoArguments::ConstantCost(get(LessThanEqualsInteger_memory_arguments)),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: get(LessThanEqualsInteger_cpu_arguments_intercept),
                    slope: get(LessThanEqualsInteger_cpu_arguments_slope),
                }),
            },
            less_than_integer: CostingFun {
                mem: TwoArguments::ConstantCost(get(LessThanInteger_memory_arguments)),
                cpu: TwoArguments::MinSize(MinSize {
                    intercept: get(LessThanInteger_cpu_arguments_intercept),
                    slope: get(LessThanInteger_cpu_arguments_slope),
                }),
            },
            list_data: CostingFun {
                mem: OneArgument::ConstantCost(get(ListData_memory_arguments)),
                cpu: OneArgument::ConstantCost(get(ListData_cpu_arguments)),
            },
            map_data: CostingFun {
                mem: OneArgument::ConstantCost(get(MapData_memory_arguments)),
                cpu: OneArgument::ConstantCost(get(MapData_cpu_arguments)),
            },
            mk_cons: CostingFun {
                mem: TwoArguments::ConstantCost(get(MkCons_memory_arguments)),
                cpu: TwoArguments::ConstantCost(get(MkCons_cpu_arguments)),
            },
            mk_nil_data: CostingFun {
                mem: OneArgument::ConstantCost(get(MkNilData_memory_arguments)),
                cpu: OneArgument::ConstantCost(get(MkNilData_cpu_arguments)),
            },
            mk_nil_pair_data: CostingFun {
                mem: OneArgument::ConstantCost(get(MkNilPairData_memory_arguments)),
                cpu: OneArgument::ConstantCost(get(MkNilPairData_cpu_arguments)),
            },
            mk_pair_data: CostingFun {
                mem: TwoArguments::ConstantCost(get(MkPairData_memory_arguments)),
                cpu: TwoArguments::ConstantCost(get(MkPairData_cpu_arguments)),
            },
            mod_integer: CostingFun {
                mem: match semantics {
                    BuiltinSemantics::A | BuiltinSemantics::B => {
                        TwoArguments::SubtractedSizes(SubtractedSizes {
                            intercept: get(ModInteger_memory_arguments_intercept),
                            minimum: get(ModInteger_memory_arguments_minimum),
                            slope: get(ModInteger_memory_arguments_slope),
                        })
                    }
                    BuiltinSemantics::D => TwoArguments::LinearInY2(SubtractedSizes {
                        intercept: get(ModInteger_memory_arguments_intercept),
                        minimum: get(ModInteger_memory_arguments_minimum),
                        slope: get(ModInteger_memory_arguments_slope),
                    }),
                    BuiltinSemantics::C | BuiltinSemantics::E => {
                        TwoArguments::LinearInY(LinearSize {
                            intercept: get(ModInteger_memory_arguments_intercept),
                            slope: get(ModInteger_memory_arguments_slope),
                        })
                    }
                },
                cpu: match semantics {
                    BuiltinSemantics::A | BuiltinSemantics::B => {
                        TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                            constant: get(ModInteger_cpu_arguments_constant),
                            model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                                intercept: get(ModInteger_cpu_arguments_model_arguments_intercept),
                                slope: get(ModInteger_cpu_arguments_model_arguments_slope),
                            })),
                        })
                    }
                    BuiltinSemantics::D => {
                        TwoArguments::AboveAndBelowDiagonal(ConstantOrTwoArguments {
                            constant: get(ModInteger_cpu_arguments_constant),
                            model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                                intercept: get(ModInteger_cpu_arguments_model_arguments_intercept),
                                slope: get(ModInteger_cpu_arguments_model_arguments_slope),
                            })),
                        })
                    }
                    BuiltinSemantics::C => {
                        TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                            constant: get(ModInteger_cpu_arguments_constant),
                            model: Box::new(TwoArguments::QuadraticInXAndY(
                                TwoArgumentsQuadraticFunction {
                                    minimum: get(ModInteger_cpu_arguments_model_arguments_minimum),
                                    coeff_00: get(ModInteger_cpu_arguments_model_arguments_c00),
                                    coeff_10: get(ModInteger_cpu_arguments_model_arguments_c10),
                                    coeff_01: get(ModInteger_cpu_arguments_model_arguments_c01),
                                    coeff_20: get(ModInteger_cpu_arguments_model_arguments_c20),
                                    coeff_11: get(ModInteger_cpu_arguments_model_arguments_c11),
                                    coeff_02: get(ModInteger_cpu_arguments_model_arguments_c02),
                                },
                            )),
                        })
                    }
                    BuiltinSemantics::E => {
                        TwoArguments::AboveAndBelowDiagonal(ConstantOrTwoArguments {
                            constant: get(ModInteger_cpu_arguments_constant),
                            model: Box::new(TwoArguments::QuadraticInXAndY(
                                TwoArgumentsQuadraticFunction {
                                    minimum: get(ModInteger_cpu_arguments_model_arguments_minimum),
                                    coeff_00: get(ModInteger_cpu_arguments_model_arguments_c00),
                                    coeff_10: get(ModInteger_cpu_arguments_model_arguments_c10),
                                    coeff_01: get(ModInteger_cpu_arguments_model_arguments_c01),
                                    coeff_20: get(ModInteger_cpu_arguments_model_arguments_c20),
                                    coeff_11: get(ModInteger_cpu_arguments_model_arguments_c11),
                                    coeff_02: get(ModInteger_cpu_arguments_model_arguments_c02),
                                },
                            )),
                        })
                    }
                },
            },
            multiply_integer: CostingFun {
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: get(MultiplyInteger_memory_arguments_intercept),
                    slope: get(MultiplyInteger_memory_arguments_slope),
                }),
                cpu: match semantics {
                    BuiltinSemantics::A => TwoArguments::AddedSizes(AddedSizes {
                        intercept: get(MultiplyInteger_cpu_arguments_intercept),
                        slope: get(MultiplyInteger_cpu_arguments_slope),
                    }),
                    BuiltinSemantics::B
                    | BuiltinSemantics::C
                    | BuiltinSemantics::D
                    | BuiltinSemantics::E => TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: get(MultiplyInteger_cpu_arguments_intercept),
                        slope: get(MultiplyInteger_cpu_arguments_slope),
                    }),
                },
            },
            null_list: CostingFun {
                mem: OneArgument::ConstantCost(get(NullList_memory_arguments)),
                cpu: OneArgument::ConstantCost(get(NullList_cpu_arguments)),
            },
            quotient_integer: CostingFun {
                mem: TwoArguments::SubtractedSizes(SubtractedSizes {
                    intercept: get(QuotientInteger_memory_arguments_intercept),
                    slope: get(QuotientInteger_memory_arguments_slope),
                    minimum: get(QuotientInteger_memory_arguments_minimum),
                }),
                cpu: match semantics {
                    BuiltinSemantics::A | BuiltinSemantics::B | BuiltinSemantics::D => {
                        TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                            constant: get(QuotientInteger_cpu_arguments_constant),
                            model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                                intercept: get(
                                    QuotientInteger_cpu_arguments_model_arguments_intercept,
                                ),
                                slope: get(QuotientInteger_cpu_arguments_model_arguments_slope),
                            })),
                        })
                    }
                    BuiltinSemantics::C | BuiltinSemantics::E => {
                        TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                            constant: get(QuotientInteger_cpu_arguments_constant),
                            model: Box::new(TwoArguments::QuadraticInXAndY(
                                TwoArgumentsQuadraticFunction {
                                    minimum: get(
                                        QuotientInteger_cpu_arguments_model_arguments_minimum,
                                    ),
                                    coeff_00: get(
                                        QuotientInteger_cpu_arguments_model_arguments_c00,
                                    ),
                                    coeff_10: get(
                                        QuotientInteger_cpu_arguments_model_arguments_c10,
                                    ),
                                    coeff_01: get(
                                        QuotientInteger_cpu_arguments_model_arguments_c01,
                                    ),
                                    coeff_20: get(
                                        QuotientInteger_cpu_arguments_model_arguments_c20,
                                    ),
                                    coeff_11: get(
                                        QuotientInteger_cpu_arguments_model_arguments_c11,
                                    ),
                                    coeff_02: get(
                                        QuotientInteger_cpu_arguments_model_arguments_c02,
                                    ),
                                },
                            )),
                        })
                    }
                },
            },
            remainder_integer: CostingFun {
                mem: match semantics {
                    BuiltinSemantics::A | BuiltinSemantics::B => {
                        TwoArguments::SubtractedSizes(SubtractedSizes {
                            intercept: get(RemainderInteger_memory_arguments_intercept),
                            minimum: get(RemainderInteger_memory_arguments_minimum),
                            slope: get(RemainderInteger_memory_arguments_slope),
                        })
                    }
                    BuiltinSemantics::D => TwoArguments::LinearInY2(SubtractedSizes {
                        intercept: get(RemainderInteger_memory_arguments_intercept),
                        minimum: get(RemainderInteger_memory_arguments_minimum),
                        slope: get(RemainderInteger_memory_arguments_slope),
                    }),
                    BuiltinSemantics::C | BuiltinSemantics::E => {
                        TwoArguments::LinearInY(LinearSize {
                            intercept: get(RemainderInteger_memory_arguments_intercept),
                            slope: get(RemainderInteger_memory_arguments_slope),
                        })
                    }
                },
                cpu: match semantics {
                    BuiltinSemantics::A | BuiltinSemantics::B | BuiltinSemantics::D => {
                        TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                            constant: get(RemainderInteger_cpu_arguments_constant),
                            model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                                intercept: get(
                                    RemainderInteger_cpu_arguments_model_arguments_intercept,
                                ),
                                slope: get(RemainderInteger_cpu_arguments_model_arguments_slope),
                            })),
                        })
                    }
                    BuiltinSemantics::C | BuiltinSemantics::E => {
                        TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                            constant: get(RemainderInteger_cpu_arguments_constant),
                            model: Box::new(TwoArguments::QuadraticInXAndY(
                                TwoArgumentsQuadraticFunction {
                                    minimum: get(
                                        RemainderInteger_cpu_arguments_model_arguments_minimum,
                                    ),
                                    coeff_00: get(
                                        RemainderInteger_cpu_arguments_model_arguments_c00,
                                    ),
                                    coeff_10: get(
                                        RemainderInteger_cpu_arguments_model_arguments_c10,
                                    ),
                                    coeff_01: get(
                                        RemainderInteger_cpu_arguments_model_arguments_c01,
                                    ),
                                    coeff_20: get(
                                        RemainderInteger_cpu_arguments_model_arguments_c20,
                                    ),
                                    coeff_11: get(
                                        RemainderInteger_cpu_arguments_model_arguments_c11,
                                    ),
                                    coeff_02: get(
                                        RemainderInteger_cpu_arguments_model_arguments_c02,
                                    ),
                                },
                            )),
                        })
                    }
                },
            },
            serialise_data: CostingFun {
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: get(SerialiseData_memory_arguments_intercept),
                    slope: get(SerialiseData_memory_arguments_slope),
                }),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: get(SerialiseData_cpu_arguments_intercept),
                    slope: get(SerialiseData_cpu_arguments_slope),
                }),
            },
            sha2_256: CostingFun {
                mem: OneArgument::ConstantCost(get(Sha2_256_memory_arguments)),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: get(Sha2_256_cpu_arguments_intercept),
                    slope: get(Sha2_256_cpu_arguments_slope),
                }),
            },
            sha3_256: CostingFun {
                mem: OneArgument::ConstantCost(get(Sha3_256_memory_arguments)),
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: get(Sha3_256_cpu_arguments_intercept),
                    slope: get(Sha3_256_cpu_arguments_slope),
                }),
            },
            slice_byte_string: CostingFun {
                mem: ThreeArguments::LinearInZ(LinearSize {
                    intercept: get(SliceByteString_memory_arguments_intercept),
                    slope: get(SliceByteString_memory_arguments_slope),
                }),
                cpu: ThreeArguments::LinearInZ(LinearSize {
                    intercept: get(SliceByteString_cpu_arguments_intercept),
                    slope: get(SliceByteString_cpu_arguments_slope),
                }),
            },
            snd_pair: CostingFun {
                mem: OneArgument::ConstantCost(get(SndPair_memory_arguments)),
                cpu: OneArgument::ConstantCost(get(SndPair_cpu_arguments)),
            },
            subtract_integer: CostingFun {
                mem: TwoArguments::MaxSize(MaxSize {
                    intercept: get(SubtractInteger_memory_arguments_intercept),
                    slope: get(SubtractInteger_memory_arguments_slope),
                }),
                cpu: TwoArguments::MaxSize(MaxSize {
                    intercept: get(SubtractInteger_cpu_arguments_intercept),
                    slope: get(SubtractInteger_cpu_arguments_slope),
                }),
            },
            tail_list: CostingFun {
                mem: OneArgument::ConstantCost(get(TailList_memory_arguments)),
                cpu: OneArgument::ConstantCost(get(TailList_cpu_arguments)),
            },
            trace: CostingFun {
                mem: TwoArguments::ConstantCost(get(Trace_memory_arguments)),
                cpu: TwoArguments::ConstantCost(get(Trace_cpu_arguments)),
            },
            un_b_data: CostingFun {
                mem: OneArgument::ConstantCost(get(UnBData_memory_arguments)),
                cpu: OneArgument::ConstantCost(get(UnBData_cpu_arguments)),
            },
            un_constr_data: CostingFun {
                mem: OneArgument::ConstantCost(get(UnConstrData_memory_arguments)),
                cpu: OneArgument::ConstantCost(get(UnConstrData_cpu_arguments)),
            },
            un_i_data: CostingFun {
                mem: OneArgument::ConstantCost(get(UnIData_memory_arguments)),
                cpu: OneArgument::ConstantCost(get(UnIData_cpu_arguments)),
            },
            un_list_data: CostingFun {
                mem: OneArgument::ConstantCost(get(UnListData_memory_arguments)),
                cpu: OneArgument::ConstantCost(get(UnListData_cpu_arguments)),
            },
            un_map_data: CostingFun {
                mem: OneArgument::ConstantCost(get(UnMapData_memory_arguments)),
                cpu: OneArgument::ConstantCost(get(UnMapData_cpu_arguments)),
            },
            verify_ecdsa_secp256k1_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(get(
                    VerifyEcdsaSecp256k1Signature_memory_arguments,
                )),
                cpu: ThreeArguments::ConstantCost(get(VerifyEcdsaSecp256k1Signature_cpu_arguments)),
            },
            verify_ed25519_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(get(VerifyEd25519Signature_memory_arguments)),
                cpu: match semantics {
                    BuiltinSemantics::A => ThreeArguments::LinearInZ(LinearSize {
                        intercept: get(VerifyEd25519Signature_cpu_arguments_intercept),
                        slope: get(VerifyEd25519Signature_cpu_arguments_slope),
                    }),
                    BuiltinSemantics::B
                    | BuiltinSemantics::C
                    | BuiltinSemantics::D
                    | BuiltinSemantics::E => ThreeArguments::LinearInY(LinearSize {
                        intercept: get(VerifyEd25519Signature_cpu_arguments_intercept),
                        slope: get(VerifyEd25519Signature_cpu_arguments_slope),
                    }),
                },
            },
            verify_schnorr_secp256k1_signature: CostingFun {
                mem: ThreeArguments::ConstantCost(get(
                    VerifySchnorrSecp256k1Signature_memory_arguments,
                )),
                cpu: ThreeArguments::LinearInY(LinearSize {
                    intercept: get(VerifySchnorrSecp256k1Signature_cpu_arguments_intercept),
                    slope: get(VerifySchnorrSecp256k1Signature_cpu_arguments_slope),
                }),
            },
            bls12_381_g1_add: CostingFun {
                cpu: TwoArguments::ConstantCost(get(Bls12_381_G1_add_cpu_arguments)),
                mem: TwoArguments::ConstantCost(get(Bls12_381_G1_add_memory_arguments)),
            },
            bls12_381_g1_compress: CostingFun {
                cpu: OneArgument::ConstantCost(get(Bls12_381_G1_compress_cpu_arguments)),
                mem: OneArgument::ConstantCost(get(Bls12_381_G1_compress_memory_arguments)),
            },
            bls12_381_g1_equal: CostingFun {
                cpu: TwoArguments::ConstantCost(get(Bls12_381_G1_equal_cpu_arguments)),
                mem: TwoArguments::ConstantCost(get(Bls12_381_G1_equal_memory_arguments)),
            },
            bls12_381_g1_hash_to_group: CostingFun {
                cpu: TwoArguments::LinearInX(LinearSize {
                    intercept: get(Bls12_381_G1_hashToGroup_cpu_arguments_intercept),
                    slope: get(Bls12_381_G1_hashToGroup_cpu_arguments_slope),
                }),
                mem: TwoArguments::ConstantCost(get(Bls12_381_G1_hashToGroup_memory_arguments)),
            },
            bls12_381_g1_neg: CostingFun {
                cpu: OneArgument::ConstantCost(get(Bls12_381_G1_neg_cpu_arguments)),
                mem: OneArgument::ConstantCost(get(Bls12_381_G1_neg_memory_arguments)),
            },
            bls12_381_g1_scalar_mul: CostingFun {
                cpu: TwoArguments::LinearInX(LinearSize {
                    intercept: get(Bls12_381_G1_scalarMul_cpu_arguments_intercept),
                    slope: get(Bls12_381_G1_scalarMul_cpu_arguments_slope),
                }),
                mem: TwoArguments::ConstantCost(get(Bls12_381_G1_scalarMul_memory_arguments)),
            },
            bls12_381_g1_uncompress: CostingFun {
                cpu: OneArgument::ConstantCost(get(Bls12_381_G1_uncompress_cpu_arguments)),
                mem: OneArgument::ConstantCost(get(Bls12_381_G1_uncompress_memory_arguments)),
            },
            bls12_381_g2_add: CostingFun {
                cpu: TwoArguments::ConstantCost(get(Bls12_381_G2_add_cpu_arguments)),
                mem: TwoArguments::ConstantCost(get(Bls12_381_G2_add_memory_arguments)),
            },
            bls12_381_g2_compress: CostingFun {
                cpu: OneArgument::ConstantCost(get(Bls12_381_G2_compress_cpu_arguments)),
                mem: OneArgument::ConstantCost(get(Bls12_381_G2_compress_memory_arguments)),
            },
            bls12_381_g2_equal: CostingFun {
                cpu: TwoArguments::ConstantCost(get(Bls12_381_G2_equal_cpu_arguments)),
                mem: TwoArguments::ConstantCost(get(Bls12_381_G2_equal_memory_arguments)),
            },
            bls12_381_g2_hash_to_group: CostingFun {
                cpu: TwoArguments::LinearInX(LinearSize {
                    intercept: get(Bls12_381_G2_hashToGroup_cpu_arguments_intercept),
                    slope: get(Bls12_381_G2_hashToGroup_cpu_arguments_slope),
                }),
                mem: TwoArguments::ConstantCost(get(Bls12_381_G2_hashToGroup_memory_arguments)),
            },
            bls12_381_g2_neg: CostingFun {
                cpu: OneArgument::ConstantCost(get(Bls12_381_G2_neg_cpu_arguments)),
                mem: OneArgument::ConstantCost(get(Bls12_381_G2_neg_memory_arguments)),
            },
            bls12_381_g2_scalar_mul: CostingFun {
                cpu: TwoArguments::LinearInX(LinearSize {
                    intercept: get(Bls12_381_G2_scalarMul_cpu_arguments_intercept),
                    slope: get(Bls12_381_G2_scalarMul_cpu_arguments_slope),
                }),
                mem: TwoArguments::ConstantCost(get(Bls12_381_G2_scalarMul_memory_arguments)),
            },
            bls12_381_g2_uncompress: CostingFun {
                cpu: OneArgument::ConstantCost(get(Bls12_381_G2_uncompress_cpu_arguments)),
                mem: OneArgument::ConstantCost(get(Bls12_381_G2_uncompress_memory_arguments)),
            },
            bls12_381_final_verify: CostingFun {
                cpu: TwoArguments::ConstantCost(get(Bls12_381_finalVerify_cpu_arguments)),
                mem: TwoArguments::ConstantCost(get(Bls12_381_finalVerify_memory_arguments)),
            },
            bls12_381_miller_loop: CostingFun {
                cpu: TwoArguments::ConstantCost(get(Bls12_381_millerLoop_cpu_arguments)),
                mem: TwoArguments::ConstantCost(get(Bls12_381_millerLoop_memory_arguments)),
            },
            bls12_381_mul_ml_result: CostingFun {
                cpu: TwoArguments::ConstantCost(get(Bls12_381_mulMlResult_cpu_arguments)),
                mem: TwoArguments::ConstantCost(get(Bls12_381_mulMlResult_memory_arguments)),
            },
            keccak_256: CostingFun {
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: get(Keccak_256_cpu_arguments_intercept),
                    slope: get(Keccak_256_cpu_arguments_slope),
                }),
                mem: OneArgument::ConstantCost(get(Keccak_256_memory_arguments)),
            },
            blake2b_224: CostingFun {
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: get(Blake2b_224_cpu_arguments_intercept),
                    slope: get(Blake2b_224_cpu_arguments_slope),
                }),
                mem: OneArgument::ConstantCost(get(Blake2b_224_memory_arguments)),
            },
            integer_to_byte_string: CostingFun {
                cpu: ThreeArguments::QuadraticInZ(QuadraticFunction {
                    coeff_0: get(IntegerToByteString_cpu_arguments_c0),
                    coeff_1: get(IntegerToByteString_cpu_arguments_c1),
                    coeff_2: get(IntegerToByteString_cpu_arguments_c2),
                }),
                mem: ThreeArguments::LiteralInYorLinearInZ(LinearSize {
                    intercept: get(IntegerToByteString_memory_arguments_intercept),
                    slope: get(IntegerToByteString_memory_arguments_slope),
                }),
            },
            byte_string_to_integer: CostingFun {
                cpu: TwoArguments::QuadraticInY(QuadraticFunction {
                    coeff_0: get(ByteStringToInteger_cpu_arguments_c0),
                    coeff_1: get(ByteStringToInteger_cpu_arguments_c1),
                    coeff_2: get(ByteStringToInteger_cpu_arguments_c2),
                }),
                mem: TwoArguments::LinearInY(LinearSize {
                    intercept: get(ByteStringToInteger_memory_arguments_intercept),
                    slope: get(ByteStringToInteger_memory_arguments_slope),
                }),
            },
            and_byte_string: CostingFun {
                cpu: ThreeArguments::LinearInYandZ(TwoVariableLinearSize {
                    intercept: get(AndByteString_cpu_arguments_intercept),
                    slope1: get(AndByteString_cpu_arguments_slope1),
                    slope2: get(AndByteString_cpu_arguments_slope2),
                }),
                mem: ThreeArguments::LinearInMaxYZ(LinearSize {
                    intercept: get(AndByteString_memory_arguments_intercept),
                    slope: get(AndByteString_memory_arguments_slope),
                }),
            },
            or_byte_string: CostingFun {
                cpu: ThreeArguments::LinearInYandZ(TwoVariableLinearSize {
                    intercept: get(OrByteString_cpu_arguments_intercept),
                    slope1: get(OrByteString_cpu_arguments_slope1),
                    slope2: get(OrByteString_cpu_arguments_slope2),
                }),
                mem: ThreeArguments::LinearInMaxYZ(LinearSize {
                    intercept: get(OrByteString_memory_arguments_intercept),
                    slope: get(OrByteString_memory_arguments_slope),
                }),
            },
            xor_byte_string: CostingFun {
                cpu: ThreeArguments::LinearInYandZ(TwoVariableLinearSize {
                    intercept: get(XorByteString_cpu_arguments_intercept),
                    slope1: get(XorByteString_cpu_arguments_slope1),
                    slope2: get(XorByteString_cpu_arguments_slope2),
                }),
                mem: ThreeArguments::LinearInMaxYZ(LinearSize {
                    intercept: get(XorByteString_memory_arguments_intercept),
                    slope: get(XorByteString_memory_arguments_slope),
                }),
            },
            complement_byte_string: CostingFun {
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: get(ComplementByteString_cpu_arguments_intercept),
                    slope: get(ComplementByteString_cpu_arguments_slope),
                }),
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: get(ComplementByteString_memory_arguments_intercept),
                    slope: get(ComplementByteString_memory_arguments_slope),
                }),
            },
            read_bit: CostingFun {
                cpu: TwoArguments::ConstantCost(get(ReadBit_cpu_arguments)),
                mem: TwoArguments::ConstantCost(get(ReadBit_memory_arguments)),
            },
            write_bits: CostingFun {
                cpu: ThreeArguments::LinearInY(LinearSize {
                    intercept: get(WriteBits_cpu_arguments_intercept),
                    slope: get(WriteBits_cpu_arguments_slope),
                }),
                mem: ThreeArguments::LinearInX(LinearSize {
                    intercept: get(WriteBits_memory_arguments_intercept),
                    slope: get(WriteBits_memory_arguments_slope),
                }),
            },
            replicate_byte: CostingFun {
                cpu: TwoArguments::LinearInX(LinearSize {
                    intercept: get(ReplicateByte_cpu_arguments_intercept),
                    slope: get(ReplicateByte_cpu_arguments_slope),
                }),
                mem: TwoArguments::LinearInX(LinearSize {
                    intercept: get(ReplicateByte_memory_arguments_intercept),
                    slope: get(ReplicateByte_memory_arguments_slope),
                }),
            },
            shift_byte_string: CostingFun {
                cpu: TwoArguments::LinearInX(LinearSize {
                    intercept: get(ShiftByteString_cpu_arguments_intercept),
                    slope: get(ShiftByteString_cpu_arguments_slope),
                }),
                mem: TwoArguments::LinearInX(LinearSize {
                    intercept: get(ShiftByteString_memory_arguments_intercept),
                    slope: get(ShiftByteString_memory_arguments_slope),
                }),
            },
            rotate_byte_string: CostingFun {
                cpu: TwoArguments::LinearInX(LinearSize {
                    intercept: get(RotateByteString_cpu_arguments_intercept),
                    slope: get(RotateByteString_cpu_arguments_slope),
                }),
                mem: TwoArguments::LinearInX(LinearSize {
                    intercept: get(RotateByteString_memory_arguments_intercept),
                    slope: get(RotateByteString_memory_arguments_slope),
                }),
            },
            count_set_bits: CostingFun {
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: get(CountSetBits_cpu_arguments_intercept),
                    slope: get(CountSetBits_cpu_arguments_slope),
                }),
                mem: OneArgument::ConstantCost(get(CountSetBits_memory_arguments)),
            },
            find_first_set_bit: CostingFun {
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: get(FindFirstSetBit_cpu_arguments_intercept),
                    slope: get(FindFirstSetBit_cpu_arguments_slope),
                }),
                mem: OneArgument::ConstantCost(get(FindFirstSetBit_memory_arguments)),
            },
            ripemd_160: CostingFun {
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: get(Ripemd_160_cpu_arguments_intercept),
                    slope: get(Ripemd_160_cpu_arguments_slope),
                }),
                mem: OneArgument::ConstantCost(get(Ripemd_160_memory_arguments)),
            },
            exp_mod_int: CostingFun {
                cpu: ThreeArguments::ExpModCost(ExpModCostingFunction {
                    coefficient_00: get(ExpModInteger_cpu_arguments_coefficient00),
                    coefficient_11: get(ExpModInteger_cpu_arguments_coefficient11),
                    coefficient_12: get(ExpModInteger_cpu_arguments_coefficient12),
                }),
                mem: ThreeArguments::LinearInZ(LinearSize {
                    intercept: get(ExpModInteger_memory_arguments_intercept),
                    slope: get(ExpModInteger_memory_arguments_slope),
                }),
            },
            drop_list: CostingFun {
                cpu: TwoArguments::LinearInX(LinearSize {
                    intercept: get(DropList_cpu_arguments_intercept),
                    slope: get(DropList_cpu_arguments_slope),
                }),
                mem: TwoArguments::ConstantCost(get(DropList_memory_arguments)),
            },
            length_of_array: CostingFun {
                cpu: OneArgument::ConstantCost(get(LengthOfArray_cpu_arguments)),
                mem: OneArgument::ConstantCost(get(LengthOfArray_memory_arguments)),
            },
            list_to_array: CostingFun {
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: get(ListToArray_cpu_arguments_intercept),
                    slope: get(ListToArray_cpu_arguments_slope),
                }),
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: get(ListToArray_memory_arguments_intercept),
                    slope: get(ListToArray_memory_arguments_slope),
                }),
            },
            index_array: CostingFun {
                cpu: TwoArguments::ConstantCost(get(IndexArray_cpu_arguments)),
                mem: TwoArguments::ConstantCost(get(IndexArray_memory_arguments)),
            },
            bls12_381_g1_multi_scalar_mul: CostingFun {
                cpu: TwoArguments::LinearInX(LinearSize {
                    intercept: get(Bls12_381_G1_multiScalarMul_cpu_arguments_intercept),
                    slope: get(Bls12_381_G1_multiScalarMul_cpu_arguments_slope),
                }),
                mem: TwoArguments::ConstantCost(get(Bls12_381_G1_multiScalarMul_memory_arguments)),
            },
            bls12_381_g2_multi_scalar_mul: CostingFun {
                cpu: TwoArguments::LinearInX(LinearSize {
                    intercept: get(Bls12_381_G2_multiScalarMul_cpu_arguments_intercept),
                    slope: get(Bls12_381_G2_multiScalarMul_cpu_arguments_slope),
                }),
                mem: TwoArguments::ConstantCost(get(Bls12_381_G2_multiScalarMul_memory_arguments)),
            },
            insert_coin: CostingFun {
                cpu: FourArguments::LinearInU(LinearSize {
                    intercept: get(InsertCoin_cpu_arguments_intercept),
                    slope: get(InsertCoin_cpu_arguments_slope),
                }),
                mem: FourArguments::LinearInU(LinearSize {
                    intercept: get(InsertCoin_memory_arguments_intercept),
                    slope: get(InsertCoin_memory_arguments_slope),
                }),
            },
            lookup_coin: CostingFun {
                cpu: ThreeArguments::LinearInZ(LinearSize {
                    intercept: get(LookupCoin_cpu_arguments_intercept),
                    slope: get(LookupCoin_cpu_arguments_slope),
                }),
                mem: ThreeArguments::ConstantCost(get(LookupCoin_memory_arguments)),
            },
            union_value: CostingFun {
                cpu: TwoArguments::WithInteractionInXAndY(TwoVariableWithInteractionSize {
                    coeff_00: get(UnionValue_cpu_arguments_c00),
                    coeff_10: get(UnionValue_cpu_arguments_c10),
                    coeff_01: get(UnionValue_cpu_arguments_c01),
                    coeff_11: get(UnionValue_cpu_arguments_c11),
                }),
                mem: TwoArguments::AddedSizes(AddedSizes {
                    intercept: get(UnionValue_memory_arguments_intercept),
                    slope: get(UnionValue_memory_arguments_slope),
                }),
            },
            value_contains: CostingFun {
                cpu: TwoArguments::ConstAboveDiagonal(ConstantOrTwoArguments {
                    constant: get(ValueContains_cpu_arguments_constant),
                    model: Box::new(TwoArguments::LinearInXAndY(TwoVariableLinearSize {
                        intercept: get(ValueContains_cpu_arguments_model_arguments_intercept),
                        slope1: get(ValueContains_cpu_arguments_model_arguments_slope1),
                        slope2: get(ValueContains_cpu_arguments_model_arguments_slope2),
                    })),
                }),
                mem: TwoArguments::ConstantCost(get(ValueContains_memory_arguments)),
            },
            value_data: CostingFun {
                cpu: OneArgument::LinearCost(LinearSize {
                    intercept: get(ValueData_cpu_arguments_intercept),
                    slope: get(ValueData_cpu_arguments_slope),
                }),
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: get(ValueData_memory_arguments_intercept),
                    slope: get(ValueData_memory_arguments_slope),
                }),
            },
            un_value_data: CostingFun {
                cpu: OneArgument::QuadraticCost(QuadraticFunction {
                    coeff_0: get(UnValueData_cpu_arguments_c0),
                    coeff_1: get(UnValueData_cpu_arguments_c1),
                    coeff_2: get(UnValueData_cpu_arguments_c2),
                }),
                mem: OneArgument::LinearCost(LinearSize {
                    intercept: get(UnValueData_memory_arguments_intercept),
                    slope: get(UnValueData_memory_arguments_slope),
                }),
            },
            scale_value: CostingFun {
                cpu: TwoArguments::LinearInY(LinearSize {
                    intercept: get(ScaleValue_cpu_arguments_intercept),
                    slope: get(ScaleValue_cpu_arguments_slope),
                }),
                mem: TwoArguments::LinearInY(LinearSize {
                    intercept: get(ScaleValue_memory_arguments_intercept),
                    slope: get(ScaleValue_memory_arguments_slope),
                }),
            },
        },
    }
}

#[derive(Debug, PartialEq, Default)]
pub struct CostingFun<T> {
    pub mem: T,
    pub cpu: T,
}

#[derive(Debug, PartialEq)]
pub enum OneArgument {
    ConstantCost(i64),
    LinearCost(LinearSize),
    QuadraticCost(QuadraticFunction),
}

impl Default for OneArgument {
    fn default() -> Self {
        OneArgument::ConstantCost(UNAVAILABLE_BUILTIN_COST_PLACEHOLDER)
    }
}

impl OneArgument {
    pub fn cost(&self, x: i64) -> i64 {
        match self {
            OneArgument::ConstantCost(c) => *c,
            OneArgument::LinearCost(m) => m.slope * x + m.intercept,
            OneArgument::QuadraticCost(q) => q.coeff_0 + (q.coeff_1 * x) + (q.coeff_2 * x * x),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TwoArguments {
    ConstantCost(i64),
    LinearInX(LinearSize),
    LinearInY(LinearSize),
    LinearInY2(SubtractedSizes),
    LinearInXAndY(TwoVariableLinearSize),
    WithInteractionInXAndY(TwoVariableWithInteractionSize),
    AddedSizes(AddedSizes),
    SubtractedSizes(SubtractedSizes),
    MultipliedSizes(MultipliedSizes),
    MinSize(MinSize),
    MaxSize(MaxSize),
    LinearOnDiagonal(ConstantOrLinear),
    ConstAboveDiagonal(ConstantOrTwoArguments),
    AboveAndBelowDiagonal(ConstantOrTwoArguments),
    ConstBelowDiagonal(ConstantOrTwoArguments),
    QuadraticInY(QuadraticFunction),
    QuadraticInXAndY(TwoArgumentsQuadraticFunction),
    ConstAboveDiagonalIntoQuadraticXAndY(i64, TwoArgumentsQuadraticFunction),
}

impl Default for TwoArguments {
    fn default() -> Self {
        TwoArguments::ConstantCost(UNAVAILABLE_BUILTIN_COST_PLACEHOLDER)
    }
}

impl TwoArguments {
    pub fn cost(&self, x: i64, y: i64) -> i64 {
        match self {
            TwoArguments::ConstantCost(c) => *c,
            // Saturating arithmetic mirrors Plutus' `SatInt` costing integers, so
            // that a literally-costed argument (e.g. `dropList`'s count) whose
            // value approaches `i64::MAX` clamps instead of overflowing.
            TwoArguments::LinearInX(l) => l.slope.saturating_mul(x).saturating_add(l.intercept),
            TwoArguments::LinearInY(l) => l.slope.saturating_mul(y).saturating_add(l.intercept),
            TwoArguments::LinearInY2(l) => l.slope.saturating_mul(y).saturating_add(l.intercept),
            TwoArguments::LinearInXAndY(l) => l.slope1 * x + l.slope2 * y + l.intercept,
            TwoArguments::WithInteractionInXAndY(l) => {
                l.coeff_00 + l.coeff_10 * x + l.coeff_01 * y + l.coeff_11 * x * y
            }
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
            TwoArguments::AboveAndBelowDiagonal(l) => {
                let p = *l.model.clone();
                p.cost(x.max(y), x.min(y))
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
            TwoArguments::QuadraticInXAndY(q) => std::cmp::max(
                q.minimum,
                q.coeff_00
                    + q.coeff_10 * x
                    + q.coeff_01 * y
                    + q.coeff_20 * x * x
                    + q.coeff_11 * x * y
                    + q.coeff_02 * y * y,
            ),
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
    ExpModCost(ExpModCostingFunction),
    LiteralInYorLinearInZ(LinearSize),
    LinearInMaxYZ(LinearSize),
    LinearInYandZ(TwoVariableLinearSize),
}

impl Default for ThreeArguments {
    fn default() -> Self {
        ThreeArguments::ConstantCost(UNAVAILABLE_BUILTIN_COST_PLACEHOLDER)
    }
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
            ThreeArguments::ExpModCost(e) => {
                let cost =
                    e.coefficient_00 + e.coefficient_11 * y * z + e.coefficient_12 * y * z * z;

                if x <= z { cost } else { cost + cost / 2 }
            }
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
pub enum FourArguments {
    ConstantCost(i64),
    LinearInU(LinearSize),
}

impl Default for FourArguments {
    fn default() -> Self {
        FourArguments::ConstantCost(UNAVAILABLE_BUILTIN_COST_PLACEHOLDER)
    }
}

impl FourArguments {
    pub fn cost(&self, _: i64, _: i64, _: i64, u: i64) -> i64 {
        match self {
            FourArguments::ConstantCost(c) => *c,
            FourArguments::LinearInU(l) => l.slope * u + l.intercept,
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
pub struct TwoVariableWithInteractionSize {
    pub coeff_00: i64,
    pub coeff_10: i64,
    pub coeff_01: i64,
    pub coeff_11: i64,
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

#[derive(Debug, PartialEq, Clone)]
pub struct ExpModCostingFunction {
    coefficient_00: i64,
    coefficient_11: i64,
    coefficient_12: i64,
}

#[repr(u8)]
#[derive(Debug, EnumIter, Display, Clone, Copy)]
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
    use crate::{
        ast::{Constant, Type},
        builtins::DefaultFunction,
        machine::{runtime::BuiltinSemantics, value::Value},
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn plutus_v3_divide_integer_always_uses_semantic_e_shape() {
        let mut costs: Vec<i64> = (0..350).collect();
        costs[49] = 85848;
        costs[50] = 123203;
        costs[51] = 7305;
        costs[52] = -900;
        costs[53] = 1716;
        costs[54] = 960;
        costs[55] = 57;
        costs[56] = 85848;

        let pv10 = initialize_cost_model(&Language::PlutusV3, &costs);
        let pv11 = initialize_cost_model_with_protocol(&Language::PlutusV3, 11, &costs);

        let pv10_cpu = pv10.builtin_costs.divide_integer.cpu.cost(1, 2);
        let pv11_cpu = pv11.builtin_costs.divide_integer.cpu.cost(1, 2);

        assert_eq!(pv10_cpu, pv11_cpu);
        assert_eq!(135188, pv10_cpu);
    }

    #[test]
    fn plutus_v2_multiply_integer_always_uses_semantic_d_shape() {
        let mut costs: Vec<i64> = (0..350).collect();
        costs[115] = 10;
        costs[116] = 3;

        let before_chang = initialize_cost_model_with_protocol(&Language::PlutusV2, 8, &costs);
        let after_chang = initialize_cost_model_with_protocol(&Language::PlutusV2, 9, &costs);
        let language_only = initialize_cost_model(&Language::PlutusV2, &costs);

        assert_eq!(
            TwoArguments::AddedSizes(AddedSizes {
                intercept: 10,
                slope: 3,
            }),
            before_chang.builtin_costs.multiply_integer.cpu
        );
        assert_eq!(
            TwoArguments::MultipliedSizes(MultipliedSizes {
                intercept: 10,
                slope: 3,
            }),
            after_chang.builtin_costs.multiply_integer.cpu
        );
        assert_ne!(
            after_chang.builtin_costs.multiply_integer.cpu,
            before_chang.builtin_costs.multiply_integer.cpu
        );
        assert_eq!(
            10 + 3 * (2 + 4),
            before_chang.builtin_costs.multiply_integer.cpu.cost(2, 4)
        );
        assert_eq!(
            10 + 3 * (2 * 4),
            after_chang.builtin_costs.multiply_integer.cpu.cost(2, 4)
        );
        assert_eq!(
            after_chang.builtin_costs.multiply_integer.cpu,
            language_only.builtin_costs.multiply_integer.cpu
        );
    }

    #[test]
    fn default_for_language_and_protocol_selects_pre_chang_costing_for_v1_and_v2() {
        for language in [Language::PlutusV1, Language::PlutusV2] {
            let before_chang = CostModel::default_for_language_and_protocol(&language, 8);
            let after_chang = CostModel::default_for_language_and_protocol(&language, 9);

            assert_eq!(
                TwoArguments::AddedSizes(AddedSizes {
                    intercept: 90434,
                    slope: 519,
                }),
                before_chang.builtin_costs.multiply_integer.cpu
            );
            assert_eq!(
                TwoArguments::MultipliedSizes(MultipliedSizes {
                    intercept: 90434,
                    slope: 519,
                }),
                after_chang.builtin_costs.multiply_integer.cpu
            );
        }
    }

    #[test]
    fn default_for_language_and_protocol_selects_pv11_costing_for_v1_and_v2() {
        for language in [Language::PlutusV1, Language::PlutusV2] {
            let builtin_costs =
                &CostModel::default_for_language_and_protocol(&language, 11).builtin_costs;

            assert_eq!(
                TwoArguments::AboveAndBelowDiagonal(ConstantOrTwoArguments {
                    constant: 85848,
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: 228465,
                        slope: 122,
                    })),
                }),
                builtin_costs.divide_integer.cpu
            );
            assert_eq!(
                TwoArguments::LinearInY2(SubtractedSizes {
                    intercept: 0,
                    slope: 1,
                    minimum: 1,
                }),
                builtin_costs.remainder_integer.mem
            );
            assert_eq!(
                TwoArguments::LinearInY2(SubtractedSizes {
                    intercept: 0,
                    slope: 1,
                    minimum: 1,
                }),
                builtin_costs.mod_integer.mem
            );
            assert_eq!(
                TwoArguments::AboveAndBelowDiagonal(ConstantOrTwoArguments {
                    constant: 85848,
                    model: Box::new(TwoArguments::MultipliedSizes(MultipliedSizes {
                        intercept: 228465,
                        slope: 122,
                    })),
                }),
                builtin_costs.mod_integer.cpu
            );
        }
    }

    #[test]
    fn plutus_v2_remainder_and_mod_integer_always_use_semantic_d_shape() {
        let costs: Vec<i64> = (0..350).collect();

        let cost_model = initialize_cost_model_with_protocol(&Language::PlutusV2, 11, &costs);
        let builtin_costs = &cost_model.builtin_costs;

        assert_eq!(
            TwoArguments::LinearInY2(SubtractedSizes {
                intercept: 130,
                minimum: 131,
                slope: 132,
            }),
            builtin_costs.remainder_integer.mem
        );
        assert_eq!(
            TwoArguments::LinearInY2(SubtractedSizes {
                intercept: 112,
                minimum: 113,
                slope: 114,
            }),
            builtin_costs.mod_integer.mem
        );
        assert_eq!(
            130 + 132 * 7,
            builtin_costs.remainder_integer.mem.cost(3, 7)
        );
    }

    #[test]
    fn plutus_v3_350_entry_cost_model_parses_pv11_tail() {
        let costs: Vec<i64> = (0..350).collect();

        let cost_model = initialize_cost_model_with_protocol(&Language::PlutusV3, 11, &costs);
        let builtin_costs = &cost_model.builtin_costs;

        assert_eq!(
            ThreeArguments::ExpModCost(ExpModCostingFunction {
                coefficient_00: 297,
                coefficient_11: 298,
                coefficient_12: 299,
            }),
            builtin_costs.exp_mod_int.cpu
        );
        assert_eq!(
            ThreeArguments::LinearInZ(LinearSize {
                intercept: 300,
                slope: 301,
            }),
            builtin_costs.exp_mod_int.mem
        );
        assert_eq!(
            TwoArguments::LinearInX(LinearSize {
                intercept: 302,
                slope: 303,
            }),
            builtin_costs.drop_list.cpu
        );
        assert_eq!(TwoArguments::ConstantCost(304), builtin_costs.drop_list.mem);
        assert_eq!(
            OneArgument::LinearCost(LinearSize {
                intercept: 307,
                slope: 308,
            }),
            builtin_costs.list_to_array.cpu
        );
        assert_eq!(
            TwoArguments::WithInteractionInXAndY(TwoVariableWithInteractionSize {
                coeff_00: 326,
                coeff_10: 327,
                coeff_01: 328,
                coeff_11: 329,
            }),
            builtin_costs.union_value.cpu
        );
        assert_eq!(
            TwoArguments::AddedSizes(AddedSizes {
                intercept: 330,
                slope: 331,
            }),
            builtin_costs.union_value.mem
        );
        assert_eq!(
            330 + 331 * (2 + 4),
            builtin_costs.union_value.mem.cost(2, 4)
        );
        assert_eq!(
            OneArgument::QuadraticCost(QuadraticFunction {
                coeff_0: 341,
                coeff_1: 342,
                coeff_2: 343,
            }),
            builtin_costs.un_value_data.cpu
        );
        assert_eq!(
            TwoArguments::LinearInY(LinearSize {
                intercept: 348,
                slope: 349,
            }),
            builtin_costs.scale_value.mem
        );
    }

    #[test]
    fn literal_cost_helpers_saturate_and_do_not_undercharge_malformed_lists() {
        let too_large = BigInt::from(u64::MAX) + BigInt::from(1u8);

        assert_eq!(i64::MAX, literal_abs_as_i64_saturating(&too_large));
        assert_eq!(
            i64::MAX,
            literal_abs_as_i64_saturating(&(-too_large.clone()))
        );

        let malformed = Value::bool(true);
        let proper_list = Value::list(
            Type::Integer,
            vec![Constant::Integer(1.into()), Constant::Integer(2.into())],
        );

        assert_eq!(1, list_len_or_value_ex_mem(&malformed));
        assert_eq!(2, list_len_or_value_ex_mem(&proper_list));
    }

    #[test]
    fn drop_list_costing_saturates_huge_literal_arguments() {
        let costs: Vec<i64> = (0..350).collect();
        let builtin_costs =
            initialize_cost_model_with_protocol(&Language::PlutusV3, 11, &costs).builtin_costs;
        let huge = BigInt::from(u64::MAX) + BigInt::from(1u8);

        let budget = builtin_costs
            .to_ex_budget(
                DefaultFunction::DropList,
                &[Value::integer(huge), Value::bool(true)],
                BuiltinSemantics::E,
            )
            .unwrap();

        assert_eq!(304, budget.mem);
        assert_eq!(i64::MAX, budget.cpu);
    }

    #[test]
    fn string_costing_uses_utf8_text_units_under_d_and_e() {
        let costs = BuiltinCosts::v3();
        let ascii_and_empty = [
            Value::string("abcd".to_string()),
            Value::string(String::new()),
        ];
        let mixed = [
            Value::string("abcd".to_string()),
            Value::string("éé".to_string()),
        ];

        assert_eq!(
            4,
            ascii_and_empty[0].to_ex_mem_with_semantics(BuiltinSemantics::C)
        );
        assert_eq!(
            1,
            ascii_and_empty[0].to_ex_mem_with_semantics(BuiltinSemantics::E)
        );
        assert_eq!(
            0,
            ascii_and_empty[1].to_ex_mem_with_semantics(BuiltinSemantics::E)
        );
        assert_eq!(
            0,
            Value::string("é".to_string()).to_ex_mem_with_semantics(BuiltinSemantics::D)
        );

        let append_c = costs
            .to_ex_budget(
                DefaultFunction::AppendString,
                &ascii_and_empty,
                BuiltinSemantics::C,
            )
            .unwrap();
        let append_e = costs
            .to_ex_budget(
                DefaultFunction::AppendString,
                &ascii_and_empty,
                BuiltinSemantics::E,
            )
            .unwrap();
        let equals_d = costs
            .to_ex_budget(DefaultFunction::EqualsString, &mixed, BuiltinSemantics::D)
            .unwrap();
        let encode_e = costs
            .to_ex_budget(
                DefaultFunction::EncodeUtf8,
                &ascii_and_empty[..1],
                BuiltinSemantics::E,
            )
            .unwrap();

        assert_eq!(costs.append_string.cpu.cost(4, 0), append_c.cpu);
        assert_eq!(costs.append_string.mem.cost(4, 0), append_c.mem);
        assert_eq!(costs.append_string.cpu.cost(1, 0), append_e.cpu);
        assert_eq!(costs.append_string.mem.cost(1, 0), append_e.mem);
        assert_eq!(costs.equals_string.cpu.cost(1, 1), equals_d.cpu);
        assert_eq!(costs.equals_string.mem.cost(1, 1), equals_d.mem);
        assert_eq!(costs.encode_utf8.cpu.cost(1), encode_e.cpu);
        assert_eq!(costs.encode_utf8.mem.cost(1), encode_e.mem);
    }
}
