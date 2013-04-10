structure AcousticModel : ACOUSTIC_MODEL =
struct

open Iterate

datatype hmmState = HmmState of {
                      pdfClass : int,
                      transitions : (int * real) vector
                  }

type topologyEntry = hmmState vector
                              
datatype topology = Topology of {
                      phones : Int32Vector.vector,
                      phone2idx : Int32Vector.vector,
                      entries : topologyEntry vector
                  }

datatype triple = Triple of {
                    phone : int,
                    hmmState : int,
                    pdf : int
                }

datatype transitionModel = TransitionModel of {
                             topology : topology,
                             triples : triple vector,
                             state2id : Int32Vector.vector,
                             id2state : Int32Vector.vector,
                             logProbs : RealVector.vector
                         }

datatype diagGmm = DiagGmm of {
                     consts : RealVector.vector,
                     weights : RealVector.vector,
                     invVars : RealArray2.array,
                     meansInvVars : RealArray2.array
                 }

datatype t = AcousticModel of transitionModel * diagGmm vector


fun readHmmEntry is =
    let
        val pdfClass = KaldiInput.readInt32 is
        val sz = KaldiInput.readInt32 is
        val transitions = Vector.tabulate (sz, 
                                           fn _ =>
                                              let
                                                  val first = KaldiInput.readInt32 is
                                                  val second = KaldiInput.readReal is
                                              in
                                                  (first, second)
                                              end)
    in
        HmmState {
            pdfClass = pdfClass, 
            transitions = transitions
        }
    end

fun readTopologyEntry is =
    let
        val sz = KaldiInput.readInt32 is
    in
        Vector.tabulate (sz, fn _ => readHmmEntry is)
    end

fun readTopology is =
    let
        val markerSt = KaldiInput.expectString (is, "<Topology> ")
        val phones = KaldiInput.readInt32Vector is
        val phone2idx = KaldiInput.readInt32Vector is
        val sz = KaldiInput.readInt32 is
        val entries = Vector.tabulate (sz, fn _ => readTopologyEntry is)
        val markerEn = KaldiInput.expectString (is, "</Topology> ")
    in
        Topology {
            phones = phones,
            phone2idx = phone2idx,
            entries = entries
        }
    end

fun readTriple is =
    let
        val phone = KaldiInput.readInt32 is
        val hmmState = KaldiInput.readInt32 is
        val pdf = KaldiInput.readInt32 is
    in
        Triple {
            phone = phone,
            hmmState = hmmState,
            pdf = pdf
        }
    end

fun readTriples is =
    let
        val markerSt = KaldiInput.expectString (is, "<Triples> ")
        val sz = KaldiInput.readInt32 is
        val ts = Vector.tabulate (sz, fn _ => readTriple is)
        val markerEd = KaldiInput.expectString (is, "</Triples> ")
    in
        ts
    end

fun readLogProbs is = 
    let
        val markerSt = KaldiInput.expectString (is, "<LogProbs> ")
        val probs = KaldiInput.readRealVector is
        val markerEd = KaldiInput.expectString (is, "</LogProbs> ")
    in
        probs
    end

fun computeDerived (topology, triples) =
    let
        fun topologyForPhone (Topology topology, phone) =
            Vector.sub (#entries topology,
                        Int32Vector.sub (#phone2idx topology, phone))

        fun numIds 0 = 1
          | numIds tstate =
            let
                val (Triple triple)= Vector.sub (triples, tstate-1)
                val phone = #phone triple
                val hmmState = #hmmState triple
                val pdf = #pdf triple
                val (HmmState state) = Vector.sub 
                                           (topologyForPhone (topology, phone),
                                            hmmState)
                val myNumIds = Vector.length (#transitions state)
            in
                myNumIds
            end

        val vstates = Int32Vector.concat 
                          (List.tabulate 
                               (1 + Vector.length triples,
                                (fn i => Int32Vector.tabulate (numIds i,
                                                               Util.const i))))
        val lids = 0 :: rev (foldl (fn (i, acc) =>
                                       (hd acc + numIds (i+1)) :: acc)
                                   [1]
                                   (List.tabulate (Vector.length triples, 
                                                   Util.identity)))

    in
        (Int32Vector.fromList lids, vstates)
    end
                      

fun readTransitionModel is =
    let
        val markerSt = KaldiInput.expectString (is, "<TransitionModel> ")
        val topology = readTopology is
        val triples = readTriples is
        val logProbs = readLogProbs is
        val markerEd = KaldiInput.expectString (is, "</TransitionModel> ")

        val (state2id, id2state) = computeDerived (topology, triples)
    in
        TransitionModel {
            topology = topology,
            triples = triples,
            logProbs = logProbs,
            state2id = state2id,
            id2state = id2state
        }
    end

fun computeConsts weights invVars meansInvVars =
    let
        val (nummix, dim) = RealArray2.dimensions invVars
        val offset = ~0.5 * Math.ln (2.0 * Math.pi) * Real.fromInt dim

        fun doGc (mix, d) = 
            let
                val iv = RealArray2.sub (invVars, mix, d)
                val miv = RealArray2.sub (meansInvVars, mix, d)
            in
                0.5 * Math.ln iv - 0.5 * miv * miv / iv
            end

        fun cleanup v = if (not o Real.isFinite) v andalso v > 0.0
                        then Real.negInf
                        else v

        fun computeConst mix =
            let
                val wv = RealVector.sub (weights, mix)
            in
            cleanup (Math.ln wv
                     + offset 
                     + foldl (op +)
                             0.0 
                             (List.tabulate (dim, fn d => doGc (mix, d))))
            end
    in
        RealVector.tabulate (nummix, computeConst)
    end
      

fun readDiagGmm is =
    let
        val markerSt = KaldiInput.expectString (is, "<DiagGMM> ")
        val markerC = KaldiInput.expectString (is, "<GCONSTS> ")
        val oldConsts = KaldiInput.readRealVector is
        val markerWght = KaldiInput.expectString (is, "<WEIGHTS> ")
        val weights = KaldiInput.readRealVector is
        val markerMIV = KaldiInput.expectString (is, "<MEANS_INVVARS> ")
        val meansInvVars = KaldiInput.readRealArray2 is
        val markerIV = KaldiInput.expectString (is, "<INV_VARS> ") 
        val invVars = KaldiInput.readRealArray2 is
        val markerEd = KaldiInput.expectString (is, "</DiagGMM> ")
        val consts = computeConsts weights invVars meansInvVars
    in
        DiagGmm {
            consts = consts,
            weights = weights,
            invVars = invVars,
            meansInvVars = meansInvVars
        }
    end
        
fun readDiagGmmVector is =
    let 
        val markerDim = KaldiInput.expectString (is, "<DIMENSION> ")
        val dim = KaldiInput.readInt32 is
        val markerNumPdfs = KaldiInput.expectString (is, "<NUMPDFS> ")
        val numPdfs = KaldiInput.readInt32 is
        val gmms = Vector.tabulate (numPdfs, fn _ => readDiagGmm is)
    in
        gmms
    end
       
fun read is = 
    let
        val binMarker = KaldiInput.readBinaryMarker is
        val transitionModel = readTransitionModel is
        val diagGmms = readDiagGmmVector is
    in
        if not (BinIO.endOfStream is)
        then raise KaldiInput.BadFile "extra material after end of acoustic model"
        else AcousticModel (transitionModel, diagGmms)
    end
        
fun tid2dgmmId (AcousticModel (TransitionModel tm, _), tid) =
    let 
        val triples = #triples tm
                               
        val id2state = #id2state tm
                                     
        val Triple tr = 
            (Vector.sub (triples, Int32Vector.sub (id2state, tid) - 1))
    in
        (**)(* print ("tid2dgmmId " ^ Int.toString tid ^ " = " ^ Int.toString (#pdf tr) ^ "\n"); *)
        #pdf tr
    end

fun gmmLogProb (DiagGmm dgmm, feas) =
    let
        val invVars = #invVars dgmm
        val meansInvVars = #meansInvVars dgmm
        val consts = #consts dgmm
                             
        val (nmix, dim) = RealArray2.dimensions invVars
                                                
        val prob = repeat 
                       (fn (m, acc) => 
                           acc +
                           Math.exp (RealVector.sub (consts, m)
                                     + repeat 
                                           (fn (i, acc2) =>
                                               let
                                                   val fv = RealVector.sub (feas, i)
                                                   val miv = RealArray2.sub (meansInvVars, m, i)
                                                   val iv = RealArray2.sub (invVars, m, i)
                                               in
                                                   acc2 
                                                   + miv * fv
                                                   - 0.5 * iv * fv * fv
                                               end)
                                           dim
                                           0.0))
                       nmix
                       0.0
    in
        Math.ln prob
    end


fun logProb _ 0 feas = Real.negInf
  | logProb am tid feas =
    let
        val AcousticModel (_, dgmms) = am
                                           
        fun tid2dgmm (AcousticModel (_, dgmms), tid) =
            Vector.sub (dgmms, tid2dgmmId (am, tid))
    in
        gmmLogProb (tid2dgmm (am, tid), feas)
    end

fun memoizeLogProb (am, feas) =
    let
        val AcousticModel (_, dgmms) = am

        val memoFn = Util.memoize
                         (fn dgmmId =>
                             gmmLogProb (Vector.sub (dgmms, dgmmId), feas))
    in
        fn tid => 
           if tid = 0 
           then Real.negInf
           else memoFn (tid2dgmmId (am, tid))
    end

end
