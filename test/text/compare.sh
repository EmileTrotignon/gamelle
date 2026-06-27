#!/usr/bin/env bash
# Render the shared scene with both backends and compare:
#   $1 raylib executable   $2 scenario name   $3 html page
#   $4 browser screenshot tool   $5 browser size-dump tool
#   $6 output filename prefix (e.g. "glyph_")
set -euo pipefail

SHOT_EXE="$1"
SCENARIO="$2"
HTML="$3"
SCREENSHOT_EXE="$4"
DUMP_EXE="$5"
P="${6:-}"

# Serialize concurrent runs (e.g. building both comparison aliases at once):
# they would otherwise fight over the X display and the geckodriver port.
exec 9>/tmp/gamelle_screenshot.lock
flock 9

# 1. raylib, rendered headlessly with software GL on a virtual X server. Also
# records its Text.size predictions (GAMELLE_SIZE_LOG).
GAMELLE_SCREENSHOT="${P}raylib.png" \
  GAMELLE_SIZE_LOG="${P}raylib_sizes.txt" \
  LIBGL_ALWAYS_SOFTWARE=1 \
  xvfb-run -w 1 -a -s "-screen 0 800x800x24" "$SHOT_EXE" "$SCENARIO"

# 2. browser, rendered headlessly with firefox via geckodriver. Only the
# geckodriver we start is ever killed (it cleans up its own firefox children);
# we never touch other firefox processes.
geckodriver --port 4444 </dev/null >/dev/null 2>&1 &
GECKO=$!
trap 'kill "$GECKO" 2>/dev/null || true' EXIT
# Wait for the driver to bind its port before connecting.
for _ in $(seq 1 40); do
  ss -ltn 2>/dev/null | grep -q ':4444 ' && break
  sleep 0.5
done
# screenshot.exe / dump_sizes.exe build file://$PWD/$HTML, so $HTML must be
# relative to here.
"$SCREENSHOT_EXE" "$HTML" >browser_raw.png
"$DUMP_EXE" "$HTML" >"${P}browser_sizes.txt"
# The element screenshot includes the 1px canvas border; crop it off.
magick browser_raw.png -shave 1x1 "${P}browser.png"

# 3. odiff (on size-matched copies; both should already be identical).
# --antialiasing ignores anti-aliased edge pixels: stb (raylib) and firefox
# rasterise glyph edges ~1px differently, which is unavoidable and would
# otherwise swamp the real signal (glyph position/size/advance misalignment).
read -r W H < <(magick identify -format '%w %h\n' "${P}raylib.png")
magick "${P}browser.png" -resize "${W}x${H}!" browser_norm.png
odiff --antialiasing "${P}raylib.png" browser_norm.png "${P}diff.png" || true
[ -f "${P}diff.png" ] || magick -size "${W}x${H}" xc:black "${P}diff.png"

# 4. native (un-resized) side-by-side so size differences stay visible.
magick montage -label raylib "${P}raylib.png" -label browser "${P}browser.png" \
  -tile 2x1 -geometry +6+6 -background gray "${P}compare.png"

# 5. unified diff of the two size-prediction dumps (any disagreement shows up as
# changed width/height values).
diff -u "${P}raylib_sizes.txt" "${P}browser_sizes.txt" \
  >"${P}sizes_diff.txt" || true
