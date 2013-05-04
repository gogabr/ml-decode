structure KaldiFuns: KALDI_FUNS =
struct

val kaldiComputeMfcc = _import "kaldi_compute_mfcc": string -> C_Pointer.t;
val kaldiMfccNumRows = _import "kaldi_mfcc_num_rows": C_Pointer.t -> int;
val kaldiMfccNumCols =  _import "kaldi_mfcc_num_cols": C_Pointer.t -> int;
val kaldiMfccGetMatrix = _import "kaldi_mfcc_get_matrix": C_Pointer.t * RealArray.array -> unit;
val kaldiMfccFree = _import "kaldi_mfcc_free": C_Pointer.t -> unit;

fun computeMfccFile fname =
    let
        val mtx = kaldiComputeMfcc (fname ^ "\000")
        val rows = kaldiMfccNumRows mtx
        val cols = kaldiMfccNumCols mtx
        val arr = RealArray.array (rows * cols, 0.0)
        val _ = kaldiMfccGetMatrix (mtx, arr)
        val _ = kaldiMfccFree mtx
    in
        List.tabulate (rows,
                       fn i => (RealArraySlice.vector o RealArraySlice.slice) (arr, cols * i, SOME cols))
    end

end
