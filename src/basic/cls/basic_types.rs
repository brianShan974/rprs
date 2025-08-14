use derive_more::Display;

#[derive(Clone, Debug, Display)]
pub enum BasicType {
    Number(NumberType),
    #[display("Boolean")]
    Boolean,
    #[display("Char")]
    Char,
    #[display("String")]
    String,
}

#[derive(Clone, Debug, Display)]
#[display("{}", _0)]
pub enum NumberType {
    SignedInteger(SignedIntegerType),
    UnsignedInteger(UnsignedIntegerType),
    FloatingPoint(FloatingPointType),
}

#[derive(Clone, Debug, Display)]
pub enum SignedIntegerType {
    #[display("Byte")]
    Byte,
    #[display("Short")]
    Short,
    #[display("Int")]
    Int,
    #[display("Long")]
    Long,
}

#[derive(Clone, Debug, Display)]
pub enum UnsignedIntegerType {
    #[display("UByte")]
    UByte,
    #[display("UShort")]
    UShort,
    #[display("UInt")]
    UInt,
    ULong,
}

#[derive(Clone, Debug, Display)]
pub enum FloatingPointType {
    #[display("Float")]
    Float,
    #[display("Double")]
    Double,
}
