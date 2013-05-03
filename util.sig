signature UTIL =
sig
    val vectorSliceToList: 'a VectorSlice.slice -> 'a list

    val const: 'a -> 'b -> 'a
    val identity: 'a -> 'a

    val printRealVector: RealVector.vector -> unit
    val printRealArray2: RealArray2.array -> unit
                                           
end
