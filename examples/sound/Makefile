.PHONY: run html

run-bin:
	dune exec --root=. ./sound_bin.exe

run:
	dune build --root=. --watch _build/default/src/sound.cmxs & \
		export DUNE_PID=$$! ; \
		gamelle hotreload _build/default/src/sound.cmxs ; \
		kill $${DUNE_PID}

html:
	dune build --root=. --profile=release sound.html
	xdg-open _build/default/sound.html
