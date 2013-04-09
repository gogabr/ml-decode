signature KALDI_INPUT =
sig

exception BadFile of string

val expectString : (BinIO.instream * string) -> unit

val readBinaryMarker : BinIO.instream -> unit

val readInt32 : BinIO.instream -> Int32.int
val readWord32 : BinIO.instream -> Word32.word
val readReal : BinIO.instream -> real
val readInt32Vector : BinIO.instream -> Int32Vector.vector
val readRealVector : BinIO.instream -> RealVector.vector
val readRealArray2 : BinIO.instream -> RealArray2.array

end
