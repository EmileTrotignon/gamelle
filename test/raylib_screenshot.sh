#!/usr/bin/env bash
# Capture a gamelle.raylib program's window to a PNG entirely outside the
# backend (the backend has no screenshot code). The program runs normally under
# a virtual X server; Xvfb mirrors that screen to an XWD-format file via -fbdir,
# which we snapshot once the program has drawn a few frames, then we stop it.
#
#   $1 output png   $2 width   $3 height   $4 Xvfb server number
#   $5.. program (exe + args)
#
# The server number is given explicitly (rather than via xvfb-run -a) so that
# several captures can run in parallel on distinct displays without racing.
#
# Window placement: the backend opens a fixed INIT x INIT window which the X
# server centres on the screen; the program then resizes it to its drawing box
# (W x H) but the top-left corner stays put. So on a SCREEN x SCREEN display the
# window sits at ((SCREEN-INIT)/2, (SCREEN-INIT)/2) with size W x H. We pick a
# screen big enough for that rectangle to fit fully, then crop it back out.
set -euo pipefail

OUT="$1"
W="$2"
H="$3"
SERVERNUM="$4"
shift 4

INIT=640 # gamelle.raylib's initial window size (raylib/gamelle_backend.ml).
SCREEN=1024
OFF=$(((SCREEN - INIT) / 2))

FBDIR="$(mktemp -d)"
trap 'rm -rf "$FBDIR"' EXIT

# GAMELLE_NO_AUDIO: there is no audio device under Xvfb; skip audio init.
GAMELLE_NO_AUDIO=1 \
  LIBGL_ALWAYS_SOFTWARE=1 \
  xvfb-run -w 1 -n "$SERVERNUM" -s "-screen 0 ${SCREEN}x${SCREEN}x24 -fbdir $FBDIR" \
  bash -c '
    out="$1"; fbdir="$2"; w="$3"; h="$4"; off="$5"; shift 5
    "$@" &
    app=$!
    # Let it open the window and render several frames (the first frame draws
    # before the window is resized to the drawing box).
    sleep 2
    # Xvfb writes screen 0 to this XWD file; crop the window region out of it.
    # Force RGBA output (PNG32): the browser PNGs have an alpha channel, so the
    # colour-comparison cram tests expect 8-digit #RRGGBBAA values from both.
    magick "xwd:$fbdir/Xvfb_screen0" -crop "${w}x${h}+${off}+${off}" +repage \
      -alpha set "PNG32:$out"
    kill "$app" 2>/dev/null || true
    wait "$app" 2>/dev/null || true
  ' _ "$OUT" "$FBDIR" "$W" "$H" "$OFF" "$@"
