structure ReadBin : READ_BIN =
struct

exception BadFile

fun rw8 is = 
    case BinIO.input1 is of
        NONE => raise BadFile
     |  SOME w => w

fun ri32l is = Int32.fromInt (Word64.toIntX (PackWord32Little.subVecX (BinIO.inputN (is, 4), 0)))

fun rw32l is = Word32.fromLarge (PackWord32Little.subVec (BinIO.inputN (is, 4), 0))

fun rf32l is = PackReal32Little.fromBytes (BinIO.inputN (is, 4))

(* inefficient *)
fun ri64l is = Int64.fromLarge (Word64.toLargeIntX (PackWord64Little.subVecX (BinIO.inputN (is, 8), 0)))

fun rw64l is = PackWord64Little.subVec (BinIO.inputN (is, 8), 0)

fun rf64l is = PackReal64Little.fromBytes (BinIO.inputN (is, 8))

end
