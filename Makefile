all: test

MLTON_DIR := /usr/lib/mlton
KALDI_DIR := /home/gogabr/src/kaldi
ATLAS_DIR := /home/gogabr/src/kaldi/tools/ATLAS

KALDI_CFLAGS := -I$(KALDI_DIR)/src -DHAVE_ATLAS -I$(ATLAS_DIR)/include -I$(MLTON_DIR)/include

CCOPT := -O3 -ffast-math

LDOPT := kaldi-interface.o \
		 $(KALDI_DIR)/src/feat/kaldi-feature.a \
	     $(KALDI_DIR)/src/matrix/kaldi-matrix.a \
	     $(KALDI_DIR)/src/base/kaldi-base.a \
		 -L$(ATLAS_DIR)/build/install/lib \
		 -llapack -lcblas -latlas \
	     -lstdc++

MLOPT := -cc-opt '$(CCOPT)' \
	     -link-opt '$(LDOPT)' \
	     -codegen c \
	     -const 'MLton.safe false' \


test: *.sig *.sml test.mlb kaldi-interface.o
	mlton $(MLOPT) test.mlb

kaldi-interface.o: kaldi-interface.cc
	gcc -c -o $@ $(CCOPT) $(KALDI_CFLAGS) $^

run:
	./test $(HOME)/data/models/config.txt ./config.txt

clean: 
	rm -f test *.o
