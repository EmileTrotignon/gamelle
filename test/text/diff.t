  $ odiff glyph_browser.png glyph_raylib.png 
  Found 687 different pixels (0.41%)
  [22]
  $ odiff roboto_glyph_browser.png roboto_glyph_raylib.png 
  Found 370 different pixels (0.22%)
  [22]
  $ odiff lines_browser.png lines_raylib.png 
  Found 14215 different pixels (3.47%)
  [22]
The roboto scene is the one exception: it is the only multi-glyph *proportional*
text, and the browser draws each glyph through the platform freetype at a
fractional CSS pixel size, whose glyph-edge antialiasing differs by Firefox
build (the raylib side is stb_truetype at whole-pixel sizes, so it is identical
everywhere). The single-glyph and monospace scenes above land byte-identical
across environments, but roboto's diff floor drifts by ~150 px, so here we only
assert we are still at the floor — "ok" below a generous threshold, the /100
count otherwise so a real regression still fails.

  $ odiff roboto_browser.png roboto_raylib.png 2>&1 | awk '/identical/{print "ok"} /different/{print ($2 < 10000 ? "ok" : int($2/100))}'
  ok
  $ odiff view_browser.png view_raylib.png
  Found 6088 different pixels (1.49%)
  [22]

  $ odiff --antialiasing glyph_browser.png glyph_raylib.png
  Images are identical
  $ odiff --antialiasing roboto_glyph_browser.png roboto_glyph_raylib.png 
  Images are identical
  $ odiff --antialiasing lines_browser.png lines_raylib.png 
  Found 3994 different pixels (0.98%)
  [22]
  $ odiff --antialiasing roboto_browser.png roboto_raylib.png 2>&1 | awk '/identical/{print "ok"} /different/{print ($2 < 4000 ? "ok" : int($2/100))}'
  ok
  $ odiff --antialiasing view_browser.png view_raylib.png
  Found 1386 different pixels (0.34%)
  [22]
