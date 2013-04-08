signature READ_BIN =
sig

    exception BadFile

    val rw8 : BinIO.instream -> Word8.word
    val rw32l : BinIO.instream -> Word32.word
    val ri32l : BinIO.instream -> Int32.int
    val rf32l : BinIO.instream -> Real32.real
    val rw64l : BinIO.instream -> Word64.word
    val ri64l : BinIO.instream -> Int64.int
    val rf64l : BinIO.instream -> Real64.real

end
