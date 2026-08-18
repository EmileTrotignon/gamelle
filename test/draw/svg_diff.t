Compare the raylib and browser renderings of the SVG scene, in units of 100
differing pixels (see diff.t for why the count is coarsened this way).

The scene draws the same vector logo plain, under a rotated view, under a concave
(star) polygon clip, zoomed in 6x, and zoomed out 6x. Both backends rasterise the
vector at display resolution: the browser natively, and raylib via nanosvg into a
texture re-rasterised on demand as the on-screen scale grows (so the 6x zoom-in
stays crisp rather than sampling a fixed load-time raster up into blur, and the
zoom-out minifies as cleanly). The two rasterisers antialias edges slightly
differently, so as elsewhere we coarsen the count and assert we are at the jitter
floor rather than an exact value.

  $ odiff svg_jsoo.png svg_raylib.png 2>&1 | awk '/identical/{print 0} /different/{print int($2/100)}'
  26

The antialiasing-filtered count sits at a low per-environment floor; as in diff.t
we assert only that we are still at it, printing "ok" below a generous threshold
and the /100 count otherwise so a real regression still fails.

  $ odiff --antialiasing svg_jsoo.png svg_raylib.png 2>&1 | awk '/identical/{print "ok"} /different/{print ($2 < 4000 ? "ok" : int($2/100))}'
  ok
