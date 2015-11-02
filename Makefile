OCAMLBUILD = ocamlbuild -classic-display -use-ocamlfind

gen_google_api:
	$(OCAMLBUILD) gen_google_api.byte

test:
	$(OCAMLBUILD) test.byte

clean:
	$(OCAMLBUILD) -clean
