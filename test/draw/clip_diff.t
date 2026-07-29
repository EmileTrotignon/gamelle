Compare the raylib and browser renderings of the clipping scene, in units of 100
differing pixels (see diff.t for why the count is coarsened this way).

The scene exercises low-vertex clips (box-under-rotation, convex hexagon, concave
star), high-vertex clips (a 100-gon, a ~130-edge raycast "visibility" polygon and
a 90-gon minimap) and a self-overlapping clip (a pentagram). The raylib backend
clips by compositing each draw through a signed-distance shader over the clip
polygon's edges.

The high-vertex clips once broke because the shader capped the edge list at 64
and left it *open* (fixed by raising the cap to 256 and closing the loop). The
pentagram exposes a second raylib-only bug: the shader decides inside/outside
with an even-odd rule, but the browser's canvas clip uses the non-zero winding
rule (its default). Where a polygon self-overlaps — the pentagram's centre
pentagon, or two crossing rays of a visibility polygon near the player — even-odd
cancels the double coverage into a hole while non-zero keeps it filled. That is
the gap oedipus showed above the player, flickering as the rays reordered.

Until the shader uses non-zero winding, the pentagram's hollow centre stands out
here (the other clips are at the antialiasing-jitter floor):

  $ odiff clip_jsoo.png clip_raylib.png 2>&1 | awk '/identical/{print 0} /different/{print int($2/100)}'
  140

  $ odiff --antialiasing clip_jsoo.png clip_raylib.png 2>&1 | awk '/identical/{print 0} /different/{print int($2/100)}'
  75
