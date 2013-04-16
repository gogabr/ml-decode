all: test

CCOPT := -O3 -ffast-math

MLOPT := -cc-opt '$(CCOPT)' -codegen c -const 'MLton.safe false'


test: *.sig *.sml test.mlb
	mlton $(MLOPT) test.mlb

run:
	./test $(HOME)/data/models/config.txt \
	       $(HOME)/data/models/final.mdl \
		   $(HOME)/data/models/HCLG-const.fst \
	       $(HOME)/data/models/word_table.txt \
		   $(HOME)/data/wav8/filmy8.mfcd

clean: 
	rm test
