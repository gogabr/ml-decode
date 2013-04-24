signature MONO_PARTITION =
sig
    structure A : MONO_ARRAY

    (* make sure i-th element of the array (by cmp) is at i-th position, with those
       less that it going before it and those greater after. *)
    val partition: (A.elem * A.elem -> order) -> A.array * int -> unit

    val checkPartition: (A.elem * A.elem -> order) -> A.array * int -> bool
end
