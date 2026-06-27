#!/usr/bin/env bash
# Render the shared scene with both backends and compare:
#   $1 raylib executable   $2 html page   $3 browser screenshot tool
#   $4 font for montage labels   $5 raylib external-screenshot helper
#   $6 geckodriver port (unique per parallel run)
set -euo pipefail

RAYLIB_EXE="$1"
HTML="$2"
SCREENSHOT_EXE="$3"
# Explicit font for the montage labels: the portable static ImageMagick used in
# CI has no fontconfig, so its default font resolves to null ("unable to read
# font `(null)'"). Passing a concrete .ttf avoids relying on system font setup.
FONT="$4"
# Helper that runs a raylib program under Xvfb and captures its window.
SHOT_HELPER="$5"
# Unique geckodriver port for this run, so several comparison rules can run in
# parallel without colliding. The Xvfb server number is derived from it.
PORT="$6"
SERVERNUM=$((PORT - 4344))

# 1. browser, rendered headlessly with firefox via geckodriver. Its canvas is
# sized to the scene's drawing box, so it also tells us how big to capture the
# raylib window. Only the geckodriver we start is ever killed (it cleans up its
# own firefox children); we never touch other firefox processes.
geckodriver --port "$PORT" </dev/null >/dev/null 2>&1 &
GECKO=$!
trap 'kill "$GECKO" 2>/dev/null || true' EXIT
# Wait for the driver to bind its port before connecting.
for _ in $(seq 1 40); do
  ss -ltn 2>/dev/null | grep -q ":${PORT} " && break
  sleep 0.5
done
# screenshot.exe / dump_sizes.exe build file://$PWD/$HTML, so $HTML must be
# relative to here.
"$SCREENSHOT_EXE" "$HTML" "$PORT" >browser_raw.png
# The element screenshot includes the 1px canvas border; crop it off.
magick browser_raw.png -shave 1x1 "jsoo.png"
read -r W H < <(magick identify -format '%w %h\n' "jsoo.png")

# 2. raylib, captured externally (the backend has no screenshot code) at the same
# size as the browser canvas.
bash "$SHOT_HELPER" raylib.png "$W" "$H" "$SERVERNUM" "$RAYLIB_EXE"

# 3. odiff (on size-matched copies; both should already be identical modulo AA).
magick "jsoo.png" -resize "${W}x${H}!" browser_norm.png
odiff "raylib.png" browser_norm.png "diff.png" || true
[ -f "diff.png" ] || magick -size "${W}x${H}" xc:black "diff.png"

# 4. native (un-resized) side-by-side so size differences stay visible.
magick montage -font "$FONT" -label raylib "raylib.png" -label browser "jsoo.png" \
  -tile 2x1 -geometry +6+6 -background gray "compare.png"

