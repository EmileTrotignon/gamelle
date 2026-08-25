.PHONY: run html

run-bin:
	dune exec --root=. ./morpion_bin.exe

run:
	dune build --root=. --watch _build/default/src/morpion.cmxs & \
		export DUNE_PID=$$! ; \
		gamelle hotreload _build/default/src/morpion.cmxs ; \
		kill $${DUNE_PID}

html:
	dune build --root=. --profile=release morpion.html
	xdg-open _build/default/morpion.html
