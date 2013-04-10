signature UTIL =
sig
    (**)
    val memoizeHits: unit -> int
    val memoizeMisses: unit -> int

    val memoize: (int -> 'a) -> int -> 'a
    val vectorToList: 'a vector -> 'a list

    val const: 'a -> 'b -> 'a
    val identity: 'a -> 'a
                                           
end
