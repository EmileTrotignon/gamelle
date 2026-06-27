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
#
# Timing: rather than capturing once after a fixed sleep, we poll the framebuffer
# until the cropped window region actually has content. A single fixed delay is
# racy in CI: the comparison rules run in parallel, so several Xvfb + software-GL
# programs contend for the CPU, and the heaviest scene can still be showing its
# initial blank/black frame when the deadline fires — yielding an all-black PNG
# and a 100%-different odiff. Polling for a non-uniform frame removes that race.
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
# GALLIUM_DRIVER=softpipe: force Mesa's reference software rasteriser instead of
# llvmpipe. llvmpipe's antialiasing depends on the LLVM version it codegens with,
# so its output drifts by a few edge pixels between Mesa/LLVM builds (a dev box
# vs CI); softpipe has no LLVM dependency, giving renders that reproduce across
# environments so the cram comparisons can pin exact pixel counts.
GAMELLE_NO_AUDIO=1 \
  LIBGL_ALWAYS_SOFTWARE=1 \
  GALLIUM_DRIVER=softpipe \
  xvfb-run -w 1 -n "$SERVERNUM" -s "-screen 0 ${SCREEN}x${SCREEN}x24 -fbdir $FBDIR" \
  bash -c '
    out="$1"; fbdir="$2"; w="$3"; h="$4"; off="$5"; shift 5
    "$@" &
    app=$!

    # Crop the window region out of the live framebuffer. Xvfb writes screen 0 to
    # this XWD file. Force RGBA output (PNG32): the browser PNGs have an alpha
    # channel, so the colour-comparison cram tests expect 8-digit #RRGGBBAA
    # values from both. -strip drops the timestamp/date chunks ImageMagick stamps
    # from the file mtime, which otherwise make every promoted PNG differ in git
    # even when the pixels are identical.
    capture() {
      magick "xwd:$fbdir/Xvfb_screen0" -crop "${w}x${h}+${off}+${off}" +repage \
        -alpha set -strip "PNG32:$out"
    }

    # Poll until the captured region has content. A blank/black frame (the window
    # before it has drawn, or before it has been resized to the drawing box) is
    # perfectly uniform, so standard_deviation == 0; any real frame is not. Give
    # it up to ~30s, then fall through with whatever we have so the comparison
    # still produces an (informative) diff rather than hanging.
    for _ in $(seq 1 60); do
      sleep 0.5
      capture 2>/dev/null || continue
      sd=$(magick "$out" -format "%[fx:standard_deviation]" info: 2>/dev/null || echo 0)
      # awk: true when sd > 0.0005 (well above floating-point noise, well below
      # any genuine multi-colour frame).
      if awk -v s="$sd" "BEGIN { exit !(s > 0.0005) }"; then
        break
      fi
    done
    # One final capture so $out reflects the latest (settled) frame.
    capture

    kill "$app" 2>/dev/null || true
    wait "$app" 2>/dev/null || true
  ' _ "$OUT" "$FBDIR" "$W" "$H" "$OFF" "$@"
