Compare the raylib and browser renderings of the clipping scene, in units of 100
differing pixels (see diff.t for why the count is coarsened this way).

The scene exercises low-vertex clips (box-under-rotation, convex hexagon, concave
star), high-vertex clips (a 100-gon, a ~400-edge raycast "visibility" polygon and
a 90-gon minimap), a self-overlapping clip (a pentagram), a scaled minimap clip
and nested clips (two overlapping boxes, and a box further narrowed by a polygon)
that check successive clips intersect — shrinking the visible zone, never
replacing the previous one. The raylib backend clips by compositing each draw
through a signed-distance shader over each clip polygon's edges, multiplying the
polygons' coverage together so only the common interior survives.

Three raylib-only clip bugs have been fixed here: the shader once capped the edge
list at 64 and left it *open* (fixed by closing the loop); it once used an
even-odd rule where the browser's canvas clip uses non-zero winding (fixed to
match); and it once passed the edges in a fixed 256-entry uniform array and
subsampled anything larger to fit — dropping corners and distorting the boundary.
A real visibility polygon has several hundred vertices (band 4 uses ~400), so
oedipus lost a wedge of the visible area next to the player where a dropped
corner let the boundary short-circuit. The edges now travel through a texture, so
every vertex is kept like the browser and the ~400-vertex blob fills completely.
Every clip is back at the antialiasing-jitter floor:

  $ odiff clip_jsoo.png clip_raylib.png 2>&1 | awk '/identical/{print 0} /different/{print int($2/100)}'
  97

The antialiasing-filtered count sits at a low per-environment floor; as in diff.t
we assert only that we are still at it (a fixed /100 bucket boundary is flaky when
the floor jitters across it), printing "ok" below a generous threshold and the
/100 count otherwise so a real regression still fails.

  $ odiff --antialiasing clip_jsoo.png clip_raylib.png 2>&1 | awk '/identical/{print "ok"} /different/{print ($2 < 3000 ? "ok" : int($2/100))}'
  ok
