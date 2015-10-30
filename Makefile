all:
	cat genapi.ml | sed 's/!!SECRET!!/$(shell cat SECRET)/g' > genapi2.ml
	ocamlbuild -use-ocamlfind -package yojson -package cohttp.lwt genapi2.byte
