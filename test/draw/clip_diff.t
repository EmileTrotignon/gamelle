Compare the raylib and browser renderings of the clipping scene, in units of 100
differing pixels (see diff.t for why the count is coarsened this way).

The scene exercises low-vertex clips (box-under-rotation, convex hexagon, concave
star), high-vertex clips (a 100-gon, a ~400-edge raycast "visibility" polygon and
a 90-gon minimap), a self-overlapping clip (a pentagram) and a scaled minimap
clip. The raylib backend clips by compositing each draw through a signed-distance
shader over the clip polygon's edges.

Two raylib-only clip bugs have already been fixed here: the shader once capped
the edge list at 64 and left it *open* (fixed by closing the loop), and it once
used an even-odd rule where the browser's canvas clip uses non-zero winding
(fixed to match). This case exposes a third: the shader passes the edges in a
fixed-size uniform array, so a polygon with more edges than the cap (256) is
subsampled to fit — dropping corners and distorting the boundary. A real
visibility polygon has several hundred vertices (band 4 uses ~400), so oedipus
lost a wedge of the visible area next to the player where a dropped corner let
the boundary short-circuit. The browser draws every vertex, so until raylib does
too the ~400-edge star clips to a shrunken, scrambled fill and stands out here:

  $ odiff clip_jsoo.png clip_raylib.png 2>&1 | awk '/identical/{print 0} /different/{print int($2/100)}'
  161

  $ odiff --antialiasing clip_jsoo.png clip_raylib.png 2>&1 | awk '/identical/{print 0} /different/{print int($2/100)}'
  86
