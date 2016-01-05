.PHONY: all main test

all: main

clean: clean-test clean-main

clean-test:
	rm -f test/*/*_actual && rm -f test/*/*_diff

clean-main:
	ocamlbuild -clean && rm -f ./ocaml-rename

test: main
	@bash ./test.sh

main:
	ocamlbuild -use-ocamlfind -lib unix -lib ocamlcommon cli_main.native && mv cli_main.native ocaml-rename