signature MATRIX =
sig

    type 'a matrix

    val create: int * int * 'a vector -> 'a matrix

    val nRows: 'a matrix -> int
    val nCols: 'a matrix -> int

    val sub: 'a matrix * int * int -> 'a

    val row: 'a matrix * int -> 'a vector
end
