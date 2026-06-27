  $ SRCROOT=$(realpath "$(cat examples_root.txt)/../..") && EXAMPLES=$SRCROOT/examples
  $ for game in $(ls $EXAMPLES); do
  >   gamelle init "$game"
  >   find "$game" -name dune | while read f; do dune format-dune-file < "$f" > "$f.fmt" && mv "$f.fmt" "$f"; done
  >   echo "=== $game ==="
  >   find "$game" -type f | sed "s|^$game/||" | sort > tmpl
  >   find "$EXAMPLES/$game" -not -path "*/_build/*" -type f | sed "s|$EXAMPLES/$game/||" | sort > exam
  >   comm -13 tmpl exam | while IFS= read -r f; do echo "EXTRA: $f"; done
  >   comm -23 tmpl exam | while IFS= read -r f; do echo "MISSING: $f"; done
  >   comm -12 tmpl exam | while IFS= read -r file; do
  >     [ "$file" = "src/$game.ml" ] && continue
  >     diff -u --label "template/$file" --label "example/$file" "$game/$file" "$EXAMPLES/$game/$file" || true
  >   done
  >   rm -rf "$game"
  > done
  === morpion ===
  EXTRA: assets/audio/balloonPop1.wav
  EXTRA: assets/audio/balloonride1.wav
  EXTRA: assets/audio/cancel1.wav
  EXTRA: assets/audio/confirm1.wav
  EXTRA: assets/circle.png
  EXTRA: assets/cross.png
  === pong ===
  === routine ===
  === snake ===
  === sound ===
  EXTRA: assets/studiokolomna_open_sky_promotional_289410.mp3
  === trigonometry_sprint ===
  === ui ===
  === volley ===
