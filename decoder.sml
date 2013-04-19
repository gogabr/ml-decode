structure Decoder: DECODER =
struct

open Iterate

type config = {
    amScale: real,
    band: int,
    beam: real
}

datatype path = Path of int * int * Fst.arc list * real

structure IntBinaryMap = BinaryMapFn(open Int
                                     type ord_key = int)

val defaultConfig = {
    amScale = 0.0571428571429,
    band = 8000,
    beam = 16.0
}

fun processConfig tokenizedLines =
    let
        fun processLine (tokens, acc) =
            let
                val {amScale = ams, band = bd, beam = bm} = acc
            in
                case tokens of
                    ["--acoustic_scale", v] => (case Real.fromString v of
                                                    NONE => acc
                                                 |  SOME rv =>
                                                    ( print ("Setting amScale = " ^ v ^ "\n")
                                                    ; {amScale = rv,
                                                       band = bd, beam = bm}))
                  | ["--max_active", v] => (case Int.fromString v of
                                                NONE => acc
                                              | SOME iv => 
                                                ( print ("Setting band = " ^ v ^ "\n")
                                                ;  {amScale = ams,
                                                    band = iv,
                                                    beam = bm}))
                  | ["--beam", v] => (case Real.fromString v of
                                          NONE => acc
                                        | SOME rv => 
                                          (print ("Setting beam = " ^ v ^ "\n")
                                          ;  {amScale = ams, band = bd,
                                              beam = rv}))
                  | _ => acc
            end
    in
        foldl processLine defaultConfig tokenizedLines
    end
                                               

fun pStart (Path (s,_,_,_)) = s
fun pEnd (Path (_,e,_,_)) = e
fun pArcs (Path (_,_,arcs,_)) = arcs
fun pWeight (Path (_,_,_,w)) = w

fun pCompare (Path (_,_,_,w1), Path (_,_,_,w2)) = Real.compare (w1, w2)
fun pLess (Path (_,_,_,w1), Path (_,_,_,w2)) = w1 < w2
fun pMin (p1, p2) =
   if pLess (p1, p2) then p1 else p2

fun zeroPath fst = Path (Fst.startState fst, Fst.startState fst, [], 0.0)

fun pathExtend (Path (s, e, oas, w), na, auxw) =
   Path (s, 
         (Fst.arcDest na),
         (na :: oas),
         (w + Fst.arcWeight na + auxw))

fun doNonEps (cfg: config, am, fst) (mfc, pl) =
   let 
       val amScale = #amScale cfg
       val logProb = AcousticModel.memoizeLogProb (am, mfc)
   in
       Vector.concat 
           (map (fn p =>
                    let
                        val outArcs = Fst.stateNonEpsArcs (fst, pEnd p)
                    in
                        Vector.map
                            (fn a =>
                                (pathExtend (p, a,
                                             (~amScale * logProb (Fst.arcILabel a - 1)))))
                            outArcs
                    end)
                   pl)
   end

fun doEps fst pl =
    let
        val ht = IntHashTable.mkTable (8000, Fail "hash")

        fun insertIfBetter np = 
            case IntHashTable.find ht (pEnd np) of
                NONE => 
                   ( IntHashTable.insert ht (pEnd np, np)
                   ; doPath np)
             |  SOME oldp =>
                   if pLess (oldp, np)
                   then ()
                   else ( IntHashTable.insert ht (pEnd np, np)
                        ; doPath np)
        and doArc p a =
            insertIfBetter (pathExtend (p, a, 0.0))
        and doPath p =
            let
                val e = pEnd p
            in
                Vector.app (doArc p) (Fst.stateEpsArcs (fst, e))
            end         
    in
        ( Vector.app insertIfBetter pl
        ; IntHashTable.listItems ht)
    end

fun prune (cfg: config) pl =
    let
        val band = #band cfg
        val beam = #beam cfg

        val len = length pl
    in
        if len < band
        then
            let
                val topScore = foldl (fn (p, mPrev) => Real.min (mPrev, pWeight p)) Real.posInf pl
            in
                List.filter (fn p => pWeight p < topScore + beam) pl
            end
        else
            let 
                val plArr = Array.fromList pl
            in
                if Array.length plArr < band
                then pl
                else 
                    ( ArrayQSort.sort pCompare plArr
                    ; let
                        val topScore = pWeight (Array.sub (plArr, 0))
                        val botScore = pWeight (Array.sub (plArr, band - 1))
                    in
                        if botScore - topScore < beam
                        then List.tabulate (band, fn i => Array.sub (plArr, i))
                        else case Array.findi (fn (_, p) => 
                                                  pWeight p - topScore >= beam) 
                                              plArr of
                                 NONE => raise Fail "prune: failure after sort"
                               | SOME (bix, _) =>
                                 List.tabulate (bix - 1, fn i => Array.sub (plArr, i))
                    end)
            end
    end

fun vtIniState (cfg: config, fst) =
   (prune cfg o doEps fst o Vector.fromList) [zeroPath fst]

fun vtStep (cfg: config, am, fst) (mfc, pl) =
   (prune cfg o doEps fst o doNonEps (cfg, am, fst)) (mfc, pl)

fun viterbi (cfg: config, am, fst) mfcl =
   foldl (vtStep (cfg, am, fst))
         (vtIniState (cfg, fst))
         mfcl

fun backtrackFinals [] = raise Size
  | backtrackFinals (h :: t) =
       foldl pMin h t

fun getFinals fst pl =
    List.mapPartial (fn (Path (st, ed, arcs, weight)) =>
                        if not (Fst.stateIsFinal (fst, ed))
                        then NONE
                        else SOME (Path (st, ed, arcs, 
                                         (weight + Fst.stateFinalWeight (fst, ed)))))
                    pl

fun backtrack fst = backtrackFinals o getFinals fst

fun pOutputs wl p =
   map (fn a => Vector.sub (wl, Fst.arcOLabel a))
       (List.filter (fn a => Fst.arcOLabel a <> 0)
                    (rev (pArcs p)))

fun decode (cfg: config, am, fst, wl) mfcl =
   (pOutputs wl o backtrack fst o viterbi (cfg, am, fst)) mfcl

end
