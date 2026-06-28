.PHONY: run run-sdl html html-zip

run-bin:
	dune exec --root=. ./volley_bin.exe

run:
	dune build --root=. --watch _build/default/src/volley.cmxs & \
		export DUNE_PID=$$! ; \
		gamelle hotreload _build/default/src/volley.cmxs ; \
		kill $${DUNE_PID}

html:
	dune build --root=. --profile=release volley.html
	xdg-open _build/default/volley.html

html-zip:
	dune build --root=. --profile=release volley.html.zip
	@echo "built _build/default/volley.html.zip"
