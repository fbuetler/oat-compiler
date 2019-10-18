all: main.native

.PHONY: test
test: main.native
	./main.native --test

.PHONY: main.native

main.native: cinterop.c
	ocamlbuild -use-ocamlfind -Is util,x86,ll,grading -libs unix,str main.native -use-menhir

.PHONY: clean
clean:
	ocamlbuild -clean
	rm -rf output a.out
