pub enum BasicType {
    Number(NumberType),
    Boolean,
    Char,
    String,
}

pub enum NumberType {
    SignedInteger(SignedIntegerType),
    UnsignedInteger(UnsignedIntegerType),
    FloatingPoint(FloatingPointType),
}

pub enum SignedIntegerType {
    Byte,
    Short,
    Int,
    Long,
}

pub enum UnsignedIntegerType {
    UByte,
    UShort,
    UInt,
    ULong,
}

pub enum FloatingPointType {
    Float,
    Double,
}
