#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include <feat/feature-mfcc.h>
#include <feat/wave-reader.h>

#include "ml-types.h"
#include "export.h"

extern "C" {

using namespace kaldi;

Pointer kaldi_compute_mfcc(Pointer a_fname)
{
    int const HEADER_SZ = 44;

    Matrix<BaseFloat> *feat_matrix = NULL;
    char *fname = (char*)a_fname;

    int fd = -1;
    off_t fsz = -1;
    int16 *raw = NULL;

    int32 nsamples = 0;

    MfccOptions mfcc_options;
    mfcc_options.use_energy = false;
    mfcc_options.frame_opts.samp_freq = 8000;

    DeltaFeaturesOptions delta_opts;

    try {
        bool binary;
        Input ki(fname, &binary);
        if (!ki.IsOpen()) {
            goto out;
        }
        
        WaveData wav;
        wav.Read(ki.Stream());

        SubVector<BaseFloat> wav_row = wav.Data().Row(0);
        
        feat_matrix = new Matrix<BaseFloat>;
        Mfcc mfcc(mfcc_options);
        Matrix<BaseFloat> intermediate;
        mfcc.Compute(wav_row, 1.0, &intermediate);
        ComputeDeltas(delta_opts, intermediate, feat_matrix);
    } catch (std::exception e) {
        if (feat_matrix) {
            delete feat_matrix;
            feat_matrix = NULL;
        }
        goto out;
    }

out:
    free(raw);
    close(fd);
    return (Pointer)feat_matrix;
}

Int32 kaldi_mfcc_num_rows(Pointer a_mtx) 
{
    Matrix<BaseFloat> *mtx = (Matrix<BaseFloat>*)a_mtx;
    return mtx->NumRows();
}

Int32 kaldi_mfcc_num_cols(Pointer a_mtx) 
{
    Matrix<BaseFloat> *mtx = (Matrix<BaseFloat>*)a_mtx;
    return mtx->NumCols();
}

void kaldi_mfcc_get_matrix(Pointer a_mtx, Pointer a_dest)
{
    Matrix<BaseFloat> *mtx = (Matrix<BaseFloat>*)a_mtx;
    double *dest = (double*)a_dest;
    int32 nrows = mtx->NumRows();
    int32 ncols = mtx->NumCols();
    for (int i = 0; i < nrows; i++) {
        for (int j = 0; j < ncols; j++) {
            dest[ncols * i + j] = (*mtx)(i, j);
        }
    }
}

void kaldi_mfcc_free(Pointer a_mtx)
{
    Matrix<BaseFloat> *mtx = (Matrix<BaseFloat>*)a_mtx;
    delete mtx;
}

}
