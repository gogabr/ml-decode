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

    fun memoizeSmall (maxArg, f) =
        let
            val tab = Array.array (maxArg, NONE)
        in
            fn x =>
               case Array.sub (tab, x) of
                   NONE => 
                       let
                           val r = f x
                       in
                           ( Array.update (tab, x, SOME r)
                           ; misses := !misses + 1
                           ; r)
                       end
                    | SOME v => ( hits := !hits + 1
                                ; v)
        end
                      

    fun memoizeHits () = !hits
    fun memoizeMisses () = !misses

end

fun vectorSliceToList sl =
    List.tabulate (VectorSlice.length sl, fn i => VectorSlice.sub (sl, i))

fun vectorToList v =
    List.tabulate (Vector.length v, fn i => Vector.sub (v,i))

fun const x y = x

fun identity x = x

fun printRealVector v = 
    ( RealVector.app (fn x => print (" " ^ Real.toString x)) v
    ; print "\n")

fun printRealArray2 a =
    Iterate.repeat (fn (i, _) => printRealVector (RealArray2.row (a, i)))
                   (RealArray2.nRows a) ()

end
