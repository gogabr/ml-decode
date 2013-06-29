This is a speech decoder written in Standard ML.
It is compiled with MLton, and uses the Kaldi library (http://kaldi.sourceforge.net) 
to convert .wav files into MFCC features.
The algorithm is state-equivalent to Kaldi's faster-decoder,
and in my tests, works about 20% slower on a large vocabulary task (Russian geographical queries).

The reason for writing this was my frustration from having to work with C++ code.

I tried reimplementing the decoder in Haskell (modifying the excellent Husky software by
Takahiro Shinozaki, http://sourceforge.net/projects/skyhusky/), but could not get reasonable performance. 
MLton proved easier to tame than GHC.

You are free to use and modify ml-decode in any way you like. Patches are welcome as well.