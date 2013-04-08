structure KaldiInput : KALDI_INPUT =
struct

exception BadFile of string

val real32toReal = Real.fromLarge IEEEReal.TO_NEAREST o Real32.toLarge 

fun expectString (is, what) =
    let
        val xx = Byte.bytesToString (BinIO.inputN (is, size what))
    in
        if xx <> what
        then raise BadFile ("expected \"" ^ what ^ "\", got \"" ^ xx ^ "\"")
        else ()
    end

fun readBinaryMarker is =
    expectString (is, "\000B")

fun readInt32 is =
    let
        val sz = ReadBin.rw8 is
    in
        if sz <> 0w4
        then raise BadFile "expected int size 4"
        else ReadBin.ri32l is
    end

fun readWord32 is =
    let
        val sz = ReadBin.rw8 is
    in
        if sz <> 0w4
        then raise BadFile "expected word size 4"
        else ReadBin.rw32l is
    end

fun readReal is =
    let
        val sz = ReadBin.rw8 is
    in
        if sz <> 0w4
        then raise BadFile "expected float size 4"
        else real32toReal (ReadBin.rf32l is)
    end

fun readInt32Vector is = 
    let 
        val sz = readInt32 is
    in
        Int32Vector.tabulate (sz, fn _ => ReadBin.ri32l is)
    end

fun readRealVector is =
    let
        val marker = expectString (is, "FV ")
        val sz = readInt32 is
    in
        RealVector.tabulate (sz, fn _ => real32toReal (ReadBin.rf32l is))
    end

fun readRealMatrix is =
    let
        val marker = expectString (is, "FM ")
        val rows = readInt32 is
        val cols = readInt32 is
    in
        Matrix.create (rows, cols, Vector.tabulate 
                                       (rows * cols,
                                        fn _ => (real32toReal (ReadBin.rf32l is))))
    end

end
