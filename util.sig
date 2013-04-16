signature UTIL =
sig
    val vectorSliceToList: 'a VectorSlice.slice -> 'a list

    val const: 'a -> 'b -> 'a
    val identity: 'a -> 'a
                                           
end
