.PHONY: run html

run-bin:
	dune exec --root=. ./pong_bin.exe

run:
	dune build --root=. --watch _build/default/src/pong.cmxs & \
		export DUNE_PID=$$! ; \
		gamelle hotreload _build/default/src/pong.cmxs ; \
		kill $${DUNE_PID}

html:
	dune build --root=. --profile=release pong.html
	xdg-open _build/default/pong.html
