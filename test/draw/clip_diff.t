Compare the raylib and browser renderings of the clipping scene, in units of 100
differing pixels (see diff.t for why the count is coarsened this way).

The scene exercises both low-vertex clips (box-under-rotation, convex hexagon,
concave star) and high-vertex clips (a 100-gon, a ~130-edge raycast "visibility"
polygon and a 90-gon minimap). The raylib backend clips by compositing each draw
through a signed-distance shader over the clip polygon's edges; earlier it capped
that edge list at 64 and, worse, left the truncated list *open*, so any polygon
with more than 64 vertices was clipped against a broken boundary — the fill leaked
out one side and vanished on another (the oedipus minimap/visibility bug). The
shader now takes up to 256 edges and always closes the loop, so all the clips
land on the same antialiasing-jitter floor as the basic scene:

  $ odiff clip_jsoo.png clip_raylib.png 2>&1 | awk '/identical/{print 0} /different/{print int($2/100)}'
  70

  $ odiff --antialiasing clip_jsoo.png clip_raylib.png 2>&1 | awk '/identical/{print 0} /different/{print int($2/100)}'
  17
