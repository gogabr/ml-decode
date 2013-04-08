signature ACOUSTIC_MODEL =
sig
    type t

    val read : BinIO.instream -> t

    val logProb : t -> int -> RealVector.vector -> real
end
