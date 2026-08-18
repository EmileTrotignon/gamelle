Compare the raylib and browser renderings of the defs/use SVG scene, in units of
100 differing pixels (see diff.t for why the count is coarsened this way).

The scene draws a single SVG (assets/defs_use.svg) whose background is a plain
<rect> drawn directly, and whose foreground art lives inside <defs><g id="art">
and is instantiated with one <use xlink:href="#art"> — exactly how the SWF->SVG
exporter (FFDec) emits every layer. nanosvg (the raylib backend) does not resolve
<use>/<defs>, so drawn as-is only the background would survive. The asset packer
therefore runs SVGs through usvg at pack time, which inlines <use>/<defs> and
flattens shapes to plain paths, so both backends now draw the whole image.

This is a regression test for that inlining: before it, the two backends differed
by the entire foreground (~8% of pixels); with it they agree down to the
antialiasing floor, exactly like svg_diff.t. If usvg inlining regresses, the
foreground disappears from raylib and this count jumps back up.

  $ odiff defs_use_jsoo.png defs_use_raylib.png 2>&1 | awk '/identical/{print 0} /different/{print int($2/100)}'
  3

The antialiasing-filtered count sits at a low per-environment floor; as in diff.t
we assert only that we are still at it, printing "ok" below a generous threshold
and the /100 count otherwise so a real regression still fails.

  $ odiff --antialiasing defs_use_jsoo.png defs_use_raylib.png 2>&1 | awk '/identical/{print "ok"} /different/{print ($2 < 4000 ? "ok" : int($2/100))}'
  ok
