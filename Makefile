.PHONY: test check

build:
	dune build
utop:
	OCAMLRUNPARAM=b dune utop src
test:
	OCAMLRUNPARAM=b dune exec test/main.exe
play:
	OCAMLRUNPARAM=b dune exec bin/main.exe
doc:
	dune build @doc
zip:
	rm -f dunesweeper.zip
	zip -r dunesweeper.zip . -x@exclude.lst

clean:
	dune clean
	rm -f dunesweeper.zip