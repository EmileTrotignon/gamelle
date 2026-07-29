Compare the raylib and browser renderings of the shared scene. Both sides jitter
a few edge pixels per environment: the raylib side is rasterised by Mesa's
software GL (llvmpipe), whose antialiasing rounds edges differently between Mesa
versions (CI's Ubuntu Mesa vs a dev box on a newer Mesa), and the browser side's
text antialiasing shifts a handful of pixels between Firefox versions. So we
coarsen the pixel difference rather than assert an exact count.

The plain (antialiasing-sensitive) count is reported in units of 100 (integer
division): fine enough to flag a real regression — e.g. a blank capture, ~100% of
600000 px = 6000 units — while absorbing sub-100px jitter.

  $ odiff jsoo.png raylib.png 2>&1 | awk '/identical/{print 0} /different/{print int($2/100)}'
  39

The antialiasing-filtered count sits near a low floor (~1200 px here) that the
per-environment jitter can nudge across any fixed bucket boundary (that is why an
exact /100 assertion here was flaky: 1199 vs 1206 flipped 11↔12). Since the line
above already catches real regressions at fine resolution, this one only asserts
we are still at that jitter floor: it prints "ok" below a generous threshold and
the /100 count otherwise, so a genuine regression still surfaces and fails.

  $ odiff --antialiasing jsoo.png raylib.png 2>&1 | awk '/identical/{print "ok"} /different/{print ($2 < 3000 ? "ok" : int($2/100))}'
  ok
