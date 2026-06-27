Compare the raylib and browser renderings of the shared scene. The raylib side
is rasterised by Mesa's software GL (llvmpipe), whose antialiasing rounds a few
edge pixels differently between Mesa versions (e.g. CI's Ubuntu Mesa vs a dev
box on a newer Mesa). So we report the pixel difference in units of 100 (integer
division by 100): the small per-environment jitter is absorbed, while a real
regression — e.g. a blank capture, ~100% of 480000 px = 4800 units — still
stands out.

  $ odiff jsoo.png raylib.png 2>&1 | awk '/identical/{print 0} /different/{print int($2/100)}'
  51

  $ odiff --antialiasing jsoo.png raylib.png 2>&1 | awk '/identical/{print 0} /different/{print int($2/100)}'
  15
