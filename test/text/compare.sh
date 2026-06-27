#!/usr/bin/env bash
# Render the shared scene with both backends and compare:
#   $1 raylib executable   $2 scenario name   $3 html page
#   $4 browser screenshot tool   $5 browser size-dump tool
#   $6 output filename prefix (e.g. "glyph_")   $7 font for montage labels
#   $8 raylib external-screenshot helper
#   $9 geckodriver port (unique per parallel run)
set -euo pipefail

SHOT_EXE="$1"
SCENARIO="$2"
HTML="$3"
SCREENSHOT_EXE="$4"
DUMP_EXE="$5"
P="${6:-}"
# Explicit font for the montage labels: the portable static ImageMagick used in
# CI has no fontconfig, so its default font resolves to null ("unable to read
# font `(null)'"). Passing a concrete .ttf avoids relying on system font setup.
FONT="$7"
# Helper that runs a raylib program under Xvfb and captures its window.
SHOT_HELPER="$8"
# Unique geckodriver port for this run, so the comparison rules can run in
# parallel without colliding. The Xvfb server number is derived from it.
PORT="$9"
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
# relative to here. Intermediate files are prefixed so parallel runs in this same
# directory do not clobber each other.
"$SCREENSHOT_EXE" "$HTML" "$PORT" >"${P}browser_raw.png"
"$DUMP_EXE" "$HTML" "$PORT" >"${P}browser_sizes.txt"
# The element screenshot includes the 1px canvas border; crop it off. -strip (here
# and below) drops the date/tIME chunks ImageMagick stamps from the file mtime,
# which would otherwise make every promoted PNG differ in git even when the pixels
# are unchanged. (odiff's own diff.png carries no such chunks.)
magick "${P}browser_raw.png" -shave 1x1 -strip "${P}browser.png"
read -r W H < <(magick identify -format '%w %h\n' "${P}browser.png")

# 2. raylib, captured externally (the backend has no screenshot code) at the same
# size as the browser canvas. The program still records its Text.size
# predictions to GAMELLE_SIZE_LOG.
GAMELLE_SIZE_LOG="${P}raylib_sizes.txt" \
  bash "$SHOT_HELPER" "${P}raylib.png" "$W" "$H" "$SERVERNUM" "$SHOT_EXE" "$SCENARIO"

# 3. odiff (on size-matched copies; both should already be identical modulo AA).
magick "${P}browser.png" -resize "${W}x${H}!" "${P}browser_norm.png"
odiff "${P}raylib.png" "${P}browser_norm.png" "${P}diff.png" || true
[ -f "${P}diff.png" ] || magick -size "${W}x${H}" xc:black -strip "${P}diff.png"

# 4. native (un-resized) side-by-side so size differences stay visible.
magick montage -font "$FONT" -label raylib "${P}raylib.png" -label browser "${P}browser.png" \
  -tile 2x1 -geometry +6+6 -background gray -strip "${P}compare.png"

# 5. unified diff of the two size-prediction dumps (any disagreement shows up as
# changed width/height values).
diff -u "${P}raylib_sizes.txt" "${P}browser_sizes.txt" \
  >"${P}sizes_diff.txt" || true
