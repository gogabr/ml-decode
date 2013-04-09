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

fun readConfig is =
    let
        fun readLines is acc =
            let
                val {amScale = ams, band = bd, beam = bm} = acc
            in
                case TextIO.inputLine is of
                    NONE => acc
                 |  SOME l =>
                    let
                        val tokens = String.tokens (fn c => Char.isSpace c orelse c = #"=") l
                    in
                        case tokens of
                            ["--acoustic_scale", v] => (case Real.fromString v of
                                                            NONE => readLines is acc
                                                         |  SOME rv =>
                                                            ( print ("Setting amScale = " ^ v ^ "\n")
                                                            ; readLines is {amScale = rv,
                                                                            band = bd, beam = bm}))
                          | ["--band", v] => (case Int.fromString v of
                                                  NONE => readLines is acc
                                                | SOME iv => 
                                                  ( print ("Setting band = " ^ v ^ "\n")
                                                  ; readLines is {amScale = ams,
                                                                  band = iv,
                                                                  beam = bm}))
                          | ["--beam", v] => (case Real.fromString v of
                                                  NONE => readLines is acc
                                                | SOME rv => 
                                                  (print ("Setting beam = " ^ v ^ "\n")
                                                  ; readLines is {amScale = ams, band = bd,
                                                                  beam = rv}))
                          | _ => readLines is acc
                    end
            end
    in
        readLines is defaultConfig
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

       val logProb = Util.memoize (fn tid =>
                                      AcousticModel.logProb am tid mfc)
   in
       List.concat 
           (map (fn p =>
                    let
                        val outArcs = Fst.stateArcs (fst, pEnd p)
                    in
                        List.mapPartial
                            (fn a =>
                                if Fst.arcIsEpsilon a
                                then NONE
                                else SOME (pathExtend (p, a,
                                                       (~amScale * logProb (Fst.arcILabel a - 1)))))
                            (Util.vectorToList outArcs)
                    end)
                   pl)
   end

fun doEps fst pl =
    let
        fun insertIfBetter (np, ibm) = 
            let
                val oop = IntBinaryMap.find (ibm, pEnd np)
            in
                case oop of
                    NONE => doPath (np, ibm)
                 |  SOME oldp => if pLess (oldp, np)
                                 then ibm
                                 else doPath (np, ibm)
            end
        and doArc p (a, ibm) =
            if (not o Fst.arcIsEpsilon) a
            then ibm
            else doPath (pathExtend (p, a, 0.0), ibm)
        and doPath (p, ibm) =
            let
                val e = pEnd p
            in
                Vector.foldl (doArc p)
                             (IntBinaryMap.insert (ibm, e, p))
                             (Fst.stateArcs (fst, e))
            end         
    in
        IntBinaryMap.listItems (foldl insertIfBetter IntBinaryMap.empty pl)
    end

fun prune (cfg: config) pl =
    let
        val band = #band cfg
        val beam = #beam cfg

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

fun fwIniState (cfg: config, fst) =
   (prune cfg o doEps fst) [zeroPath fst]

fun fwStep (cfg: config, am, fst) (mfc, pl) =
   (prune cfg o doEps fst o doNonEps (cfg, am, fst)) (mfc, pl)

fun forward (cfg: config, am, fst) mfcl =
   foldl (fwStep (cfg, am, fst))
         (fwIniState (cfg, fst))
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
   (pOutputs wl o backtrack fst o forward (cfg, am, fst)) mfcl

end
