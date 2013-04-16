structure Util : UTIL =
struct

fun vectorSliceToList sl =
    List.tabulate (VectorSlice.length sl, fn i => VectorSlice.sub (sl, i))

fun const x y = x

fun identity x = x

end
