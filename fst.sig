signature FST =
sig
    exception BadFile of string

    type header
    type fst

    type arc

    val readHeader: BinIO.instream -> header
    val headerToString: header -> string

    val header: fst -> header
    val nStates: fst -> int
    val startState: fst -> int
                                
    val stateIsFinal: fst * int -> bool
    val stateFinalWeight: fst * int -> real
    val stateEpsArcs: fst * int -> arc vector
    val stateNonEpsArcs: fst * int -> arc vector

    val arcIsEpsilon: arc -> bool
    val arcDest: arc -> int
    val arcILabel: arc -> int
    val arcOLabel: arc -> int
    val arcWeight: arc -> real

    val readFst: BinIO.instream -> fst
                                        
end
