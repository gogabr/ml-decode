structure Util : UTIL =
struct

fun vectorSliceToList sl =
    List.tabulate (VectorSlice.length sl, fn i => VectorSlice.sub (sl, i))

fun const x y = x

fun identity x = x

fun printRealVector v = 
    ( RealVector.app (fn x => print (" " ^ Real.toString x)) v
    ; print "\n")

fun printRealArray2 a =
    Iterate.repeat (fn (i, _) => printRealVector (RealArray2.row (a, i)))
                   (RealArray2.nRows a) ()

end
