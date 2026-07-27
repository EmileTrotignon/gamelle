Compare the raylib and browser renderings of the clipping scene, in units of 100
differing pixels (see diff.t for why the count is coarsened this way).

The low-vertex clips (box-under-rotation, convex hexagon, concave star) agree
between the backends. The high-vertex clips do NOT: the raylib backend passes at
most 64 polygon edges to its clip shader and, worse, leaves the truncated edge
list open, so a clip polygon with more than 64 vertices (a 100-gon, a ~130-edge
raycast "visibility" polygon, a 90-gon minimap) is clipped against a broken,
open boundary — the fill leaks out one side and vanishes on another. This is the
oedipus minimap/visibility bug: its visibility polygons have well over 64
vertices.

Once the clip shader handles arbitrarily-many-edged polygons, this drops to the
same antialiasing-jitter floor as diff.t. Until then it records the bug:

  $ odiff clip_jsoo.png clip_raylib.png 2>&1 | awk '/identical/{print 0} /different/{print int($2/100)}'
  380

  $ odiff --antialiasing clip_jsoo.png clip_raylib.png 2>&1 | awk '/identical/{print 0} /different/{print int($2/100)}'
  310
