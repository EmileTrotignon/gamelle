.PHONY: run html

run-bin:
	dune exec --root=. ./routine_bin.exe

run:
	dune build --root=. --watch _build/default/src/routine.cmxs & \
		export DUNE_PID=$$! ; \
		gamelle hotreload _build/default/src/routine.cmxs ; \
		kill $${DUNE_PID}

html:
	dune build --root=. --profile=release routine.html
	xdg-open _build/default/routine.html
