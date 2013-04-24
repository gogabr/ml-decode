structure Decoder: DECODER =
struct

open Iterate

type config = {
    amScale: real,
    band: int,
    beam: real,
    beamStep: real
}

datatype path = Path of int * int * Fst.arc list * real

structure RealArrayPartition = MonoPartitionFn(open RealArray
                                               type elem = real
                                               type array = RealArray.array
                                               type vector = RealVector.vector)

structure RA = RealArray
structure RAS = RealArraySlice

val defaultConfig = {
    amScale = 0.0571428571429,
    band = 8000,
    beam = 16.0,
    beamStep = 0.5
}

fun processConfig tokenizedLines =
    let
        fun processLine (tokens, 
                         acc as {amScale, band, beam, beamStep}) =
            case tokens of
                ["--acoustic_scale", v] => (case Real.fromString v of
                                                NONE => acc
                                             |  SOME rv =>
                                                ( print ("Setting amScale = " ^ v ^ "\n")
                                                ; {amScale = rv,
                                                   band = band, 
                                                   beam = beam,
                                                   beamStep = beamStep}))
              | ["--max_active", v] => (case Int.fromString v of
                                            NONE => acc
                                          | SOME iv => 
                                            ( print ("Setting band = " ^ v ^ "\n")
                                            ;  {amScale = amScale,
                                                band = iv,
                                                beam = beam,
                                                beamStep = beamStep}))
              | ["--beam", v] => (case Real.fromString v of
                                      NONE => acc
                                    | SOME rv => 
                                      ( print ("Setting beam = " ^ v ^ "\n")
                                      ;  {amScale = amScale, band = band,
                                          beam = rv,
                                          beamStep = beamStep}))
              | ["--beam_step", v] => (case Real.fromString v of
                                           NONE => acc
                                         | SOME rv => ( print ("setting beamStep = " ^ v ^ "\n")
                                                      ; {amScale = amScale, band = band,
                                                         beam = beam,
                                                         beamStep = rv}))

              | _ => acc
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

fun doArcs (cfg: config, am, fst) (mfc, (beam, pl)) =
   let 
       val amScale = #amScale cfg
       val beamLimit = #beamStep cfg + beam
       val logProb = AcousticModel.memoizeLogProb (am, mfc)

       val ht = IntHashTable.mkTable (8000, Fail "hash")

       fun pIsViable (topScore, p) =
           pWeight p - topScore <  beamLimit

       fun doNonEpsArc (p, a, topSoFar) =
           let
               val np = pathExtend (p, a,
                                    (~amScale * logProb (Fst.arcILabel a - 1)))
           in
               if pIsViable (topSoFar, np)
               then ( insertIfBetter np
                    ; Real.min (topSoFar, pWeight np))
               else topSoFar
           end
       and insertIfBetter np = 
           case IntHashTable.find ht (pEnd np) of
               NONE => 
                 ( IntHashTable.insert ht (pEnd np, np)
                 ; doEpsArcsForPath np)
            |  SOME oldp =>
                 if pLess (oldp, np)
                 then ()
                 else ( IntHashTable.insert ht (pEnd np, np)
                      ; doEpsArcsForPath np)
        and doEpsArc p a =
            insertIfBetter (pathExtend (p, a, 0.0))
        and doEpsArcsForPath p =
            let
                val e = pEnd p
            in
                Vector.app (doEpsArc p) (Fst.stateEpsArcs (fst, e))
            end         
   in
       ( foldl (fn (p, topSoFar) =>
                   let
                       val outArcs = Fst.stateNonEpsArcs (fst, pEnd p)
                   in
                       Vector.foldl
                           (fn (a, topSoFar') => doNonEpsArc (p, a, topSoFar'))
                           topSoFar outArcs
                   end)
               Real.posInf pl
       ; IntHashTable.listItems ht)
   end

fun prune (cfg: config) pl =
    let
        val band = #band cfg
        val beam = #beam cfg

        val len = length pl
    in
        if len <= band
        then
            let
                val topScore = foldl (fn (p, mPrev) => Real.min (mPrev, pWeight p)) Real.posInf pl
                val resPl = List.filter (fn p => pWeight p < topScore + beam) pl
                val botScore = foldl (fn (p, mPrev) => Real.max (mPrev, pWeight p)) Real.negInf resPl
            in
                (botScore - topScore, resPl)
            end
        else
            let 
                val wArr = RA.array (len, 0.0)
                val _ = List.foldl (fn (p, i) =>
                                        ( RA.update (wArr, i, pWeight p)
                                        ; i+1))
                                   0
                                   pl
            in
                ( RealArrayPartition.partition Real.compare (wArr, band)
                ; let
                    val topScore = RAS.foldl Real.min Real.posInf (RAS.slice (wArr, 0, SOME band))
                    val botScore = RA.sub (wArr, band)
                    val cutoff = Real.min (topScore + beam, botScore)
                  in
                      (cutoff - topScore, List.filter (fn p => pWeight p <= cutoff) pl)
                  end)
            end
    end

(* Too much code copying from doArcs. Need to merge *)
fun doEps fst p =
    let
        val ht = IntHashTable.mkTable (8000, Fail "hash")

        fun insertIfBetter np = 
            case IntHashTable.find ht (pEnd np) of
                NONE => 
                   ( IntHashTable.insert ht (pEnd np, np)
                   ; doEpsArcsForPath np)
             |  SOME oldp =>
                   if pLess (oldp, np)
                   then ()
                   else ( IntHashTable.insert ht (pEnd np, np)
                        ; doEpsArcsForPath np)
        and doEpsArc p a =
            insertIfBetter (pathExtend (p, a, 0.0))
        and doEpsArcsForPath p =
            let
                val e = pEnd p
            in
                Vector.app (doEpsArc p) (Fst.stateEpsArcs (fst, e))
            end         
    in
        ( insertIfBetter p
        ; IntHashTable.listItems ht)
    end

fun vtIniState (cfg: config, fst) =
    let 
        val (_, pl) = (prune cfg o doEps fst) (zeroPath fst)
    in
        (#beam cfg, pl)
    end

fun vtStep (cfg: config, am, fst) (arg as (mfc, (beam, pl))) =
   (prune cfg o doArcs (cfg, am, fst)) arg

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
    let 
        val (_, rpl) = viterbi (cfg, am, fst) mfcl
    in
        (pOutputs wl o backtrack fst) rpl
    end

end
