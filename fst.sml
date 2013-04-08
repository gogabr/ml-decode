structure Fst : FST = 
struct

exception BadFile of string

type baseReal = Real32.real

type header = {
    fstType : string,
    arcType : string,
    version : int,
    flags   : Word32.word,
    properties : Word64.word,
    start   : int,
    numStates : int,
    numArcs : int
}

type state = {
    finalWeight : baseReal,
    pos : int,
    narcs : int,
    nieps : int,
    noeps : int
}

type arc = {
    ilabel : int,
    olabel : int,
    weight : baseReal,
    dest   : int
}

type fst = {
    header : header,
    states : state vector,
    arcs : arc vector
}

val magic = 0w2125659606


(* if andb (#flags fh) alignmentFlag, sections of the fila are aligned *)
val alignmentFlag = 0w4
val alignment = 16

(* any better way? *)
val baseRealToReal = Real.fromLarge IEEEReal.TO_NEAREST o Real32.toLarge

fun readFstString is =
    let
        val len = ReadBin.ri32l is
    in
        Vector.map Byte.byteToChar (BinIO.inputN (is, len))
    end

fun alignUp (is, fh) =
    let
        val pos = BinIO.StreamIO.filePosIn (BinIO.getInstream is)
        val rmd = Int64.toInt (pos mod alignment)
    in
        if rmd <> 0 andalso Word32.andb (#flags fh, alignmentFlag) <> 0w0
        then (BinIO.inputN (is, Int64.toInt alignment - rmd); ())
        else ()
    end

fun readHeader is = 
    let
        val magicOk = if ReadBin.rw32l is <> magic
                      then raise BadFile "Bad magic"
                      else ()
        val fstType = readFstString is
        val arcType = readFstString is
        val version = ReadBin.ri32l is
        val flags = ReadBin.rw32l is
        val properties = ReadBin.rw64l is
        val start = Int64.toInt (ReadBin.ri64l is)
        val numStates = Int64.toInt (ReadBin.ri64l is)
        val numArcs = Int64.toInt (ReadBin.ri64l is)
    in
        {
            fstType = fstType,
            arcType = arcType,
            version = version,
            flags = flags,
            properties = properties,
            start = start,
            numStates = numStates,
            numArcs = numArcs
        }
    end

fun headerToString (fh: header) =
    "{ fstType = " ^ #fstType fh 
    ^ ", arcType = " ^ #arcType fh 
    ^ ", version = " ^ Int.toString (#version fh) 
    ^ ", flags = " ^ Word32.toString (#flags fh) 
    ^ ", properties = " ^ Word64.toString (#properties fh) 
    ^ ", start = " ^ Int.toString (#start fh) 
    ^ ", numStates = " ^ Int.toString (#numStates fh) 
    ^ ", numArcs = " ^ Int.toString (#numArcs fh) 
    ^ "}"

(* This would not be needed if we had getPosIn *)
fun headerSize (fh: header) =
    4 									(* magic *)
    + 4 + String.size (#fstType fh) 	(* fstType *)
    + 4 + String.size (#arcType fh)		(* arcType *)
    + 4            						(* version *)
    + 4									(* flags *)
    + 8									(* properties *)
    + 8									(* start *)
    + 8									(* numStates *)
    + 8									(* numArcs *)

fun readState is = 
    let
        val fw = ReadBin.rf32l is
        val pos = ReadBin.ri32l is
        val narcs = ReadBin.ri32l is
        val nieps = ReadBin.ri32l is
        val noeps = ReadBin.ri32l is
    in
        {
          finalWeight = fw,
          pos = pos,
          narcs = narcs,
          nieps = nieps,
          noeps = noeps
        }
    end

fun readStates (is, fh: header) = 
    Vector.tabulate (#numStates fh, fn _ => readState is)

fun readArc is =
    let
        val ilabel = ReadBin.ri32l is
        val olabel = ReadBin.ri32l is
        val weight = ReadBin.rf32l is
        val dest = ReadBin.ri32l is
    in
        {
          ilabel = ilabel,
          olabel = olabel,
          weight = weight,
          dest = dest
        }
    end

fun readArcs (is, fh:header ) = 
    Vector.tabulate (#numArcs fh, fn _ => readArc is)

fun readFst is =
    let
        val fh = readHeader is
        val hsz = BinIO.StreamIO.filePosIn (BinIO.getInstream is)
        val hpad = alignUp (is, fh)
        val states = readStates (is, fh)
        val spad = alignUp (is, fh)
        val arcs = readArcs (is, fh)
    in
        if not (BinIO.endOfStream is)
        then raise BadFile "extra material after end of fst"
        else
            {
              header = fh,
              states = states,
              arcs = arcs
            }
    end

fun header (fst: fst) = #header fst

fun nStates (fst: fst) = (#numStates o #header) fst
                  
fun startState (fst: fst) = (#start o #header) fst

fun stateFinalWeight (fst: fst, sid) =
    let
        val states = #states fst
        val fw = #finalWeight (Vector.sub (states, sid))
    in
        baseRealToReal fw
    end

fun stateIsFinal (fst: fst, sid) =
    Real.isFinite (stateFinalWeight (fst, sid))

fun stateArcs (fst: fst, sid) =
    let
        val states = #states fst
        val arcs = #arcs fst
        val state = Vector.sub (states, sid)
    in
        VectorSlice.vector (VectorSlice.slice (arcs, #pos state, SOME (#narcs state)))
    end

fun arcDest (arc: arc) = #dest arc
fun arcILabel (arc: arc) = #ilabel arc
fun arcOLabel (arc: arc) = #olabel arc
fun arcWeight (arc: arc) = baseRealToReal (#weight arc)

fun arcIsEpsilon a = arcILabel a = 0    

end
