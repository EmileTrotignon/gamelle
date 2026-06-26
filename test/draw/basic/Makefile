.PHONY: run html

run:
	dune build --root=. --watch _build/default/src/basic.cmxs & \
		export DUNE_PID=$$! ; \
		gamelle hotreload _build/default/src/basic.cmxs ; \
		kill $${DUNE_PID}

html:
	dune build --root=. --profile=release
	xdg-open _build/default/basic.html
