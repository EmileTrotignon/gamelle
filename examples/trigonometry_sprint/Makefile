.PHONY: run html

run-bin:
	dune exec --root=. ./trigonometry_sprint_bin.exe

run:
	dune build --root=. --watch _build/default/src/trigonometry_sprint.cmxs & \
		export DUNE_PID=$$! ; \
		gamelle hotreload _build/default/src/trigonometry_sprint.cmxs ; \
		kill $${DUNE_PID}

html:
	dune build --root=. --profile=release trigonometry_sprint.html
	xdg-open _build/default/trigonometry_sprint.html
