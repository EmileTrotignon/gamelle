.PHONY: run run-sdl html

run-bin:
	dune exec --root=. ./volley_multi_bin.exe

run:
	dune build --root=. --watch _build/default/src/volley_multi.cmxs & \
		export DUNE_PID=$$! ; \
		gamelle hotreload _build/default/src/volley_multi.cmxs ; \
		kill $${DUNE_PID}

html:
	dune build --root=. --profile=release volley_multi.html
	xdg-open _build/default/volley_multi.html
