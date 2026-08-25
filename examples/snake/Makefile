.PHONY: run html

run-bin:
	dune exec --root=. ./snake_bin.exe

run:
	dune build --root=. --watch _build/default/src/snake.cmxs & \
		export DUNE_PID=$$! ; \
		gamelle hotreload _build/default/src/snake.cmxs ; \
		kill $${DUNE_PID}

html:
	dune build --root=. --profile=release snake.html
	xdg-open _build/default/snake.html
