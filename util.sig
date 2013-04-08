signature UTIL =
sig
    val memoize: (int -> 'a) -> int -> 'a
    val vectorToList: 'a vector -> 'a list
                                           
end
