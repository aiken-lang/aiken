use strum_macros::EnumString;

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, EnumString)]
#[strum(serialize_all = "camelCase")]
pub enum DefaultFunction {
    // Integer functions
    AddInteger,
    SubtractInteger,
    MultiplyInteger,
    DivideInteger,
    QuotientInteger,
    RemainderInteger,
    ModInteger,
    EqualsInteger,
    LessThanInteger,
    LessThanEqualsInteger,
    // ByteString functions
    AppendByteString,
    ConsByteString,
    SliceByteString,
    LengthOfByteString,
    IndexByteString,
    EqualsByteString,
    LessThanByteString,
    LessThanEqualsByteString,
    // Cryptography and hash functions
    #[strum(serialize = "sha2_256")]
    Sha2_256,
    Sha3_256,
    Blake2b_256,
    VerifySignature,
    VerifyEcdsaSecp256k1Signature,
    VerifySchnorrSecp256k1Signature,
    // String functions
    AppendString,
    EqualsString,
    EncodeUtf8,
    DecodeUtf8,
    // Bool function
    IfThenElse,
    // Unit function
    ChooseUnit,
    // Tracing function
    Trace,
    // Pairs functions
    FstPair,
    SndPair,
    // List functions
    ChooseList,
    MkCons,
    HeadList,
    TailList,
    NullList,
    // Data functions
    // It is convenient to have a "choosing" function for a data type that has more than two
    // constructors to get pattern matching over it and we may end up having multiple such data
    // types, hence we include the name of the data type as a suffix.
    ChooseData,
    ConstrData,
    MapData,
    ListData,
    IData,
    BData,
    UnConstrData,
    UnMapData,
    UnListData,
    UnIData,
    UnBData,
    EqualsData,
    SerialiseData,
    // Misc constructors
    // Constructors that we need for constructing e.g. Data. Polymorphic builtin
    // constructors are often problematic (See note [Representable built-in
    // functions over polymorphic built-in types])
    MkPairData,
    MkNilData,
    MkNilPairData,
}
