#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include <feat/feature-mfcc.h>

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

    fd = open(fname, O_RDONLY);
    if (fd < 0) {
        goto out;
    }

    /* TODO: take WAV properties into account */

    fsz = lseek(fd, 0, SEEK_END);
    if (fsz < 0) {
        goto out;
    }

    nsamples = (fsz - HEADER_SZ) / sizeof(int16);

    if (HEADER_SZ != lseek(fd, HEADER_SZ, SEEK_SET)) {
        goto out;
    }
    
    raw = (int16*)calloc(nsamples, sizeof(int16));
    if (nsamples * sizeof(int16) != read(fd, raw, nsamples * sizeof(int16))) {
        goto out;
    }

    try {
        Vector<BaseFloat> wavf(nsamples);
        BaseFloat *wavd = wavf.Data();
        for (int i = 0; i < nsamples; i++) {
            wavd[i] = (BaseFloat)raw[i];
        }
        
        feat_matrix = new Matrix<BaseFloat>;
        Mfcc mfcc(mfcc_options);
        Matrix<BaseFloat> intermediate;
        mfcc.Compute(wavf, 1.0, &intermediate);
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
