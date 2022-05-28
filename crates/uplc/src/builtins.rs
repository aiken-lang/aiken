use strum_macros::EnumString;

#[repr(u8)]
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, EnumString)]
#[strum(serialize_all = "camelCase")]
pub enum DefaultFunction {
    // Integer functions
    AddInteger = 0,
    SubtractInteger = 1,
    MultiplyInteger = 2,
    DivideInteger = 3,
    QuotientInteger = 4,
    RemainderInteger = 5,
    ModInteger = 6,
    EqualsInteger = 7,
    LessThanInteger = 8,
    LessThanEqualsInteger = 9,
    // ByteString functions
    AppendByteString = 10,
    ConsByteString = 11,
    SliceByteString = 12,
    LengthOfByteString = 13,
    IndexByteString = 14,
    EqualsByteString = 15,
    LessThanByteString = 16,
    LessThanEqualsByteString = 17,
    // Cryptography and hash functions
    #[strum(serialize = "sha2_256")]
    Sha2_256 = 18,
    Sha3_256 = 19,
    Blake2b_256 = 20,
    VerifySignature = 21,
    VerifyEcdsaSecp256k1Signature = 22,
    VerifySchnorrSecp256k1Signature = 23,
    // String functions
    AppendString = 24,
    EqualsString = 25,
    EncodeUtf8 = 26,
    DecodeUtf8 = 27,
    // Bool function
    IfThenElse = 28,
    // Unit function
    ChooseUnit = 29,
    // Tracing function
    Trace = 30,
    // Pairs functions
    FstPair = 31,
    SndPair = 32,
    // List functions
    ChooseList = 33,
    MkCons = 34,
    HeadList = 35,
    TailList = 36,
    NullList = 37,
    // Data functions
    // It is convenient to have a "choosing" function for a data type that has more than two
    // constructors to get pattern matching over it and we may end up having multiple such data
    // types, hence we include the name of the data type as a suffix.
    ChooseData = 38,
    ConstrData = 39,
    MapData = 40,
    ListData = 41,
    IData = 42,
    BData = 43,
    UnConstrData = 44,
    UnMapData = 45,
    UnListData = 46,
    UnIData = 47,
    UnBData = 48,
    EqualsData = 49,
    SerialiseData = 50,
    // Misc constructors
    // Constructors that we need for constructing e.g. Data. Polymorphic builtin
    // constructors are often problematic (See note [Representable built-in
    // functions over polymorphic built-in types])
    MkPairData = 51,
    MkNilData = 52,
    MkNilPairData = 53,
}
