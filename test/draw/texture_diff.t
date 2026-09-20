Compare the raylib and browser renderings of the texture scene, in units of 100
differing pixels (see diff.t for why the count is coarsened this way).

The scene pre-renders sprites into [Texture.t]s and draws them plain, under a
rotated view, zoomed in 3x, zoomed out 3x, layered against out-of-texture shapes,
clipped from the inside to a concave star, and (a third sprite) wiped and redrawn.
The sprite contents themselves are drawn under internal view transforms (rotate,
translate, scale), which must render into the texture identically on both backends. raylib renders
the sprite into an FBO texture, the browser into an offscreen canvas; the two
rasterise (and antialias) edges slightly differently, and the FBO has no MSAA, so
— as with the svg scene, which is likewise almost all antialiased edges under
rotation and zoom — we coarsen the count and only assert we are at the
per-environment jitter floor rather than an exact value. A real regression (a cell
dropping out, the z-order collapsing wrong, a vertical flip, or the inside-clip
degrading to its bounding box because the FBO lost its stencil buffer) moves tens
of thousands of pixels and still fails.

  $ odiff texture_jsoo.png texture_raylib.png 2>&1 | awk '/identical/{print "ok"} /different/{print ($2 < 12000 ? "ok" : int($2/100))}'
  ok

  $ odiff --antialiasing texture_jsoo.png texture_raylib.png 2>&1 | awk '/identical/{print "ok"} /different/{print ($2 < 12000 ? "ok" : int($2/100))}'
  ok
