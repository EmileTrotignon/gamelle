.PHONY: run html

run-bin:
	dune exec --root=. ./ui_bin.exe

run:
	dune build --root=. --watch _build/default/src/ui.cmxs & \
		export DUNE_PID=$$! ; \
		gamelle hotreload _build/default/src/ui.cmxs ; \
		kill $${DUNE_PID}

html:
	dune build --root=. --profile=release ui.html
	xdg-open _build/default/ui.html
