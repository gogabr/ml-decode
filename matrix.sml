structure Matrix : MATRIX =
struct

datatype 'a matrix = Matrix of int * int * 'a vector

fun create (r, c, v) = Matrix (r, c, v)

fun nRows (Matrix (r, _, _)) = r
fun nCols (Matrix (_, c, _)) = c

fun sub (Matrix (r, c, v), i, j) = Vector.sub (v, i * c + j)

fun row (Matrix (r, c, v), i) = VectorSlice.vector (VectorSlice.slice (v, i * c, SOME c))

end
