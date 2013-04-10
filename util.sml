structure Util : UTIL =
struct

local
    val hits = ref 0
    val misses = ref 0
in
    fun memoize f =
        let
            val ht = IntHashTable.mkTable (1000, Fail "hash")
        in
            fn x =>
               case IntHashTable.find ht x of
                   NONE => 
                   let
                       val r = f x
                   in
                       ( IntHashTable.insert ht (x, r)
                       ; misses := !misses + 1
                       ; r)
                   end
                | SOME v =>  ( hits := !hits + 1
                             ; v)
    end

    fun memoizeHits () = !hits
    fun memoizeMisses () = !misses

end

fun vectorToList v =
    List.tabulate (Vector.length v, fn i => Vector.sub (v,i))

fun const x y = x

fun identity x = x

end
