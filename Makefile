all: test

CCOPT := -O3 -ffast-math

MLOPT := -cc-opt '$(CCOPT)' -codegen c -const 'MLton.safe false'


test: *.sig *.sml test.mlb
	mlton $(MLOPT) test.mlb

run:
	./test $(HOME)/data/models/config.txt ./config.txt

clean: 
	rm -f test
