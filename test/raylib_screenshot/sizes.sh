#!/usr/bin/env bash
# Render the scene with both backends and assert their recorded text-size
# predictions (Text.size / Text.size_multiline) are byte-identical. Lighter than
# compare.sh (no PNG/montage/odiff), used as the `dune runtest` assertion. Exits
# non-zero (printing the diff) on any mismatch.
#   $1 raylib executable   $2 scenario name   $3 html page
#   $4 browser size-dump tool   $5 output filename prefix
set -euo pipefail

SHOT_EXE="$1"
SCENARIO="$2"
HTML="$3"
DUMP_EXE="$4"
P="$5"

# Serialize concurrent runs: they share the X display and geckodriver port.
exec 9>/tmp/gamelle_screenshot.lock
flock 9

# raylib: size predictions via GAMELLE_SIZE_LOG. GAMELLE_SCREENSHOT makes the run
# loop render a frame and exit; that PNG is a throwaway.
GAMELLE_SCREENSHOT="${P}throwaway.png" GAMELLE_SIZE_LOG="${P}raylib.txt" \
  LIBGL_ALWAYS_SOFTWARE=1 \
  xvfb-run -a -s "-screen 0 800x800x24" "$SHOT_EXE" "$SCENARIO"

# browser: size predictions via geckodriver + headless firefox. Only the
# geckodriver we start is killed (it cleans up its own firefox children).
geckodriver --port 4444 </dev/null >/dev/null 2>&1 &
GECKO=$!
trap 'kill "$GECKO" 2>/dev/null || true' EXIT
for _ in $(seq 1 40); do
  ss -ltn 2>/dev/null | grep -q ':4444 ' && break
  sleep 0.5
done
"$DUMP_EXE" "$HTML" >"${P}browser.txt"

# The assertion: both backends must predict the same text sizes.
diff -u "${P}raylib.txt" "${P}browser.txt"
