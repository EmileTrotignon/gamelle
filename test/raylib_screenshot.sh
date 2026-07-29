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
# Window placement: the backend opens the window directly at its drawing-box
# size W x H (GAMELLE_WINDOW_SIZE, set below), which GLFW centres on the screen.
# So on a SCREEN x SCREEN display the window sits at ((SCREEN-W)/2, (SCREEN-H)/2)
# with size W x H. We pick a screen big enough for that rectangle to fit fully,
# then crop it back out.
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

# The screen must fit the centred W x H window: (SCREEN-W)/2 + W <= SCREEN holds
# for any SCREEN >= W (likewise H), so SCREEN >= max(W, H) suffices. Keep a
# margin and at least 1024.
MAX=$((W > H ? W : H))
SCREEN=$((MAX + 256))
[ "$SCREEN" -lt 1024 ] && SCREEN=1024
OFFX=$(((SCREEN - W) / 2))
OFFY=$(((SCREEN - H) / 2))

FBDIR="$(mktemp -d)"
trap 'rm -rf "$FBDIR"' EXIT

# GAMELLE_NO_AUDIO: there is no audio device under Xvfb; skip audio init.
# GALLIUM_DRIVER=softpipe: force Mesa's reference software rasteriser instead of
# llvmpipe. llvmpipe's antialiasing depends on the LLVM version it codegens with,
# so its output drifts by a few edge pixels between Mesa/LLVM builds (a dev box
# vs CI); softpipe has no LLVM dependency, giving renders that reproduce across
# environments so the cram comparisons can pin exact pixel counts.
# GAMELLE_WINDOW_SIZE: open the window at the capture size directly, so the very
# first frame is already the right size and placement (no one-frame resize
# transient to accidentally capture — see raylib/gamelle_backend.ml).
# -w 10: seconds to allow Xvfb to come up before xvfb-run gives up on it. The
# comparison rules run in parallel, so several software-GL programs and their
# Xvfb servers start at once and contend for the CPU; under that load a software
# Xvfb can take a couple of seconds just to bind its socket. A tight ceiling (the
# old -w 1) made xvfb-run intermittently declare Xvfb "failed to start" in CI, so
# we keep a wide margin — on a fast, unloaded start xvfb-run returns as soon as
# the server is ready, so the ceiling costs nothing there.
# -e /dev/stderr: route Xvfb's own output (and xauth errors) to stderr instead of
# the default /dev/null, so a genuine startup failure shows *why* in the CI log
# (e.g. a display-lock collision) rather than a bare "failed to start".
GAMELLE_NO_AUDIO=1 \
  GAMELLE_WINDOW_SIZE="${W}x${H}" \
  LIBGL_ALWAYS_SOFTWARE=1 \
  GALLIUM_DRIVER=softpipe \
  xvfb-run -w 10 -e /dev/stderr -n "$SERVERNUM" -s "-screen 0 ${SCREEN}x${SCREEN}x24 -fbdir $FBDIR" \
  bash -c '
    out="$1"; fbdir="$2"; w="$3"; h="$4"; offx="$5"; offy="$6"; shift 6
    "$@" &
    app=$!

    # Crop the window region out of the live framebuffer. Xvfb writes screen 0 to
    # this XWD file. Force RGBA output (PNG32): the browser PNGs have an alpha
    # channel, so the colour-comparison cram tests expect 8-digit #RRGGBBAA
    # values from both. -strip drops the timestamp/date chunks ImageMagick stamps
    # from the file mtime, which otherwise make every promoted PNG differ in git
    # even when the pixels are identical.
    capture() {
      magick "xwd:$fbdir/Xvfb_screen0" -crop "${w}x${h}+${offx}+${offy}" +repage \
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
    # The poll breaks on the first non-blank frame; give the scene one more
    # moment to reach a steady state, then take the final capture.
    sleep 0.5
    capture

    kill "$app" 2>/dev/null || true
    wait "$app" 2>/dev/null || true
  ' _ "$OUT" "$FBDIR" "$W" "$H" "$OFFX" "$OFFY" "$@"
