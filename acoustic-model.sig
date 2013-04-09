signature ACOUSTIC_MODEL =
sig
    type t

    val read: BinIO.instream -> t

    val logProb: t -> int -> RealVector.vector -> real

    (* During decoding, logProb is called repeatedly with the ame model
     * and feature vector, but different transition IDs. It makes sense to memoize
     * by transition ID.
     *)
    val memoizeLogProb: (t * RealVector.vector) -> int -> real

end
