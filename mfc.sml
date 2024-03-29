structure Mfc : MFC =
struct

type mfc = RealVector.vector

fun read is = 
    let
        val binaryMarker = KaldiInput.readBinaryMarker is
        val mtx = KaldiInput.readRealArray2 is
    in
        List.tabulate (RealArray2.nRows mtx,
                       fn i => RealArray2.row (mtx, i))
    end

fun subCmn [] = []
  | subCmn mfcs =
    let
        val (_, avgs) = 
            List.foldl (fn (e, (i, acc)) =>
                           (i+1,
                            RealVector.tabulate 
                                (RealVector.length e,
                                 fn j =>
                                    RealVector.sub (acc, j) * Real.fromInt i / Real.fromInt (i + 1)
                                    + RealVector.sub (e, j) / Real.fromInt (i + 1))))
                       (0, (RealVector.tabulate (RealVector.length (hd mfcs), fn _ => 0.0)))
                       mfcs
    in
        List.map (fn e =>
                     RealVector.tabulate (RealVector.length e,
                                          fn i =>
                                             RealVector.sub (e, i)
                                             - RealVector.sub (avgs, i)))
                 mfcs
    end
end
