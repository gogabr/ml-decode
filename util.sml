structure Util : UTIL =
struct

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
                      ; r)
                  end
            |  SOME v => v
    end

fun vectorToList v =
    List.tabulate (Vector.length v, fn i => Vector.sub (v,i))

end
