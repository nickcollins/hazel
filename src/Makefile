all: www/hazel.js
www/hazel.js: hazel.byte
	js_of_ocaml +weak.js -o www/hazel.js hazel.byte
hazel.byte: hazel.re semantics.re chrome.re action_palette.re view.re general_util.re js_util.re model.re pretty.re
	scripts/refmt.sh
	rebuild -use-ocamlfind -pkgs js_of_ocaml,js_of_ocaml.ppx,js_of_ocaml.tyxml,tyxml,react,reactiveData,camomile hazel.byte
semantics.re: semantics.v
	scripts/extract-coq.sh
clean:
	rm -rf _build
	rm -f *.glob *.vo *.byte www/hazel.js semantics.re

