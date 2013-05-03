structure Decoder: DECODER =
struct

open Iterate

type config = {
    amScale: real,
    band: int,
    beam: real,
    beamDelta: real
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
    beamDelta = 0.5
}

fun processConfig tokenizedLines =
    let
        fun processLine (tokens, 
                         acc as {amScale, band, beam, beamDelta}) =
            case tokens of
                ["--acoustic_scale", v] => (case Real.fromString v of
                                                NONE => acc
                                             |  SOME rv =>
                                                ( print ("Setting amScale = " ^ v ^ "\n")
                                                ; {amScale = rv,
                                                   band = band, 
                                                   beam = beam,
                                                   beamDelta = beamDelta}))
              | ["--max_active", v] => (case Int.fromString v of
                                            NONE => acc
                                          | SOME iv => 
                                            ( print ("Setting band = " ^ v ^ "\n")
                                            ;  {amScale = amScale,
                                                band = iv,
                                                beam = beam,
                                                beamDelta = beamDelta}))
              | ["--beam", v] => (case Real.fromString v of
                                      NONE => acc
                                    | SOME rv => 
                                      ( print ("Setting beam = " ^ v ^ "\n")
                                      ;  {amScale = amScale, band = band,
                                          beam = rv,
                                          beamDelta = beamDelta}))
              | ["--beam_delta", v] => (case Real.fromString v of
                                           NONE => acc
                                         | SOME rv => ( print ("setting beamDelta = " ^ v ^ "\n")
                                                      ; {amScale = amScale, band = band,
                                                         beam = beam,
                                                         beamDelta = rv}))

              | _ => acc
    in
        foldl processLine defaultConfig tokenizedLines
    end
                                               

fun pStart (Path (s,_,_,_)) = s
fun pEnd (Path (_,e,_,_)) = e
fun pArcs (Path (_,_,arcs,_)) = arcs
fun pWeight (Path (_,_,_,w)) = w

(* Used in folds as worst possible value *)
val dummyPath = Path (0, 0, [], Real.posInf)

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

fun doArcs (cfg: config, am, fst) (mfc, (topPath, adaptiveBeam, pl)) =
   let 
       val amScale = #amScale cfg
       val logProb = AcousticModel.memoizeLogProb (am, mfc)

       val ht = IntHashTable.mkTable (8000, Fail "hash")
                                     
       val cutoff =
           let
               val bestOutgoingWeight =
                       Vector.foldl (fn (a, old) => 
                                        Real.min (old,
                                                  Fst.arcWeight a +
                                                  (~amScale * logProb (Fst.arcILabel a))))
                                    Real.posInf (Fst.stateNonEpsArcs (fst, pEnd topPath))

           in
               pWeight topPath + bestOutgoingWeight + adaptiveBeam
           end

       fun scoreIsViable score =
           score <  cutoff
                        
       fun doNonEpsArc p a =
           if scoreIsViable (pWeight p + Fst.arcWeight a)
           then
               let
                   val np = pathExtend (p, a,
                                        (~amScale * logProb (Fst.arcILabel a)))
               in
                   if scoreIsViable (pWeight np)
                   then insertIfBetter np
                   else ()
               end
           else ()
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
            if pWeight p + Fst.arcWeight a < cutoff
            then insertIfBetter (pathExtend (p, a, 0.0))
            else ()
        and doEpsArcsForPath p =
            let
                val e = pEnd p
            in
                Vector.app (doEpsArc p) (Fst.stateEpsArcs (fst, e))
            end         
   in
       ( app (fn p =>
                   let
                       val outArcs = Fst.stateNonEpsArcs (fst, pEnd p)
                   in
                       Vector.app (doNonEpsArc p) outArcs
                   end)
             pl
       ; IntHashTable.listItems ht)
   end

fun prune (cfg: config) pl =
    let
        val band = #band cfg
        val beam = #beam cfg
        val beamDelta = #beamDelta cfg

        val len = length pl

        val topPath = foldl (fn (p, mPrev) =>
                                if pWeight p < pWeight mPrev then p else mPrev)
                            dummyPath pl
        val topScore = pWeight topPath
    in
        if len <= band
        then
            let
                val resPl = List.filter (fn p => pWeight p < topScore + beam) pl
            in
                (topPath, beam, resPl)
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
                    val botScore = RA.sub (wArr, band)
                    val cutoff = Real.min (topScore + beam, botScore)
                    val adaptiveBeam = Real.min (beam, cutoff - topScore + beamDelta)
                  in
                      (topPath, adaptiveBeam, List.filter (fn p => pWeight p <= cutoff) pl)
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
    (prune cfg o doEps fst) (zeroPath fst)

fun vtStep (cfg: config, am, fst) (arg as (mfc, (topPath, adaptiveBeam, pl))) =
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
        val (_, _, rpl) = viterbi (cfg, am, fst) mfcl
    in
        String.concatWith " " ((pOutputs wl o backtrack fst) rpl)
    end

end
