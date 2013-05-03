signature UTIL =
sig
    (**)
    val memoizeHits: unit -> int
    val memoizeMisses: unit -> int

    val memoize: (int -> 'a) -> int -> 'a
    val memoizeSmall: (int * (int -> 'a)) -> int -> 'a

    val vectorSliceToList: 'a VectorSlice.slice -> 'a list
    val vectorToList: 'a vector -> 'a list

    val const: 'a -> 'b -> 'a
    val identity: 'a -> 'a

    val printRealVector: RealVector.vector -> unit
    val printRealArray2: RealArray2.array -> unit
                                           
end
