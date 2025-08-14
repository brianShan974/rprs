#[derive(Clone, Debug)]
pub enum BasicType {
    Number(NumberType),
    Boolean,
    Char,
    String,
}

#[derive(Clone, Debug)]
pub enum NumberType {
    SignedInteger(SignedIntegerType),
    UnsignedInteger(UnsignedIntegerType),
    FloatingPoint(FloatingPointType),
}

#[derive(Clone, Debug)]
pub enum SignedIntegerType {
    Byte,
    Short,
    Int,
    Long,
}

#[derive(Clone, Debug)]
pub enum UnsignedIntegerType {
    UByte,
    UShort,
    UInt,
    ULong,
}

#[derive(Clone, Debug)]
pub enum FloatingPointType {
    Float,
    Double,
}
