Compare the raylib and browser renderings of the defs/use SVG scene, in units of
100 differing pixels (see diff.t for why the count is coarsened this way).

The scene draws a single SVG (assets/defs_use.svg) whose background is a plain
<rect> drawn directly, and whose foreground art lives inside <defs><g id="art">
and is instantiated with one <use xlink:href="#art"> — exactly how the SWF->SVG
exporter (FFDec) emits every layer. The browser resolves <use> and draws the art
on top of the background; the raylib backend rasterises via nanosvg, which does
NOT resolve <use>/<defs>, so only the background survives.

This is a BUG-REPRODUCTION test: the two backends currently disagree by the whole
foreground, so the diff is large. Once nanosvg's <use> handling is fixed this
count should collapse to the antialiasing floor (like svg_diff.t), at which point
this file should be updated to assert the floor instead.

  $ odiff --antialiasing defs_use_jsoo.png defs_use_raylib.png 2>&1 | awk '/identical/{print "BUG-FIXED: identical"} /different/{print ($2 > 4000 ? "BUG: foreground missing in raylib" : "near-floor: " int($2/100))}'
  BUG: foreground missing in raylib
