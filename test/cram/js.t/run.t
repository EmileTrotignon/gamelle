  $ dune build
  $ geckodriver > /dev/null 2> /dev/null &
  $ export GECKOPID=$!
  $ ls _build/default/basic
  META.basic
  assets
  basic.dune-package
  basic.html
  basic.install
  basic.template.html
  basic_bin.exe
  basic_bin.ml
  basic_bin.mli
  basic_js.bc.js
  basic_js.ml
  basic_js.mli
  src
  $ ../screenshot/screenshot.exe _build/default/basic/basic.html > image.png
Compare against the reference screenshot (promote reference.png when intentionally changing the rendering)

  $ odiff reference.png image.png diff.png || true
  Images are identical

Check the canvas size matches the View.drawing_box dimensions (800x600)

  $ magick identify -format "size: %wx%h\n" image.png
  size: 802x602

Count pixels per exact color, filtering out colors with fewer than 500 pixels
(anti-aliasing noise). Each significant color should correspond to a specific
draw call:
background:          Color.(rgb 40 40 40)  = #282828FF
labels (Text.draw):  Color.white           = #FFFFFFFF
Segment.draw:        Color.cyan            = #00FFFFFF
Segment.draw:        Color.orange          = #FF8000FF
Box.fill:            Color.blue            = #0000FFFF
Box.draw:            Color.yellow          = #FFFF00FF
Circle.fill:         Color.red             = #FF0000FF
Circle.draw:         Color.lime            = #32CD32FF
Polygon.fill:        Color.magenta         = #FF00FFFF
Polygon.draw:        Color.gold            = #FFD700FF
Box.fill (touch TL): Color.teal            = #008080FF
Box.fill (touch TR): Color.coral           = #FF7F50FF
Box.fill (touch BL): Color.indigo          = #4B0082FF
Box.fill (touch BR): Color.violet          = #EE82EEFF
Text.draw:           Color.crimson         = #DC143CFF
Bitmap.draw:         Assets.camel (camel.png, mixed colors)

  $ magick image.png txt:- | awk 'NR>1{print $3}' | sort | uniq -c | sort -rn | awk '$1 >= 500'
   322658 #282828FF
    64172 #0000FFFF
    16236 #FF7F50FF
    16236 #EE82EEFF
    16236 #4B0082FF
    16236 #008080FF
    11723 #FF0000FF
     5974 #FF00FFFF
     2804 #000000FF
      565 #C5A97FFF
      549 #CAAF83FF
      526 #2A2AD5FF

  $ sha512sum image.png
  bf33b3d2cdaef52d7d487a29bdad603d8960185463d0c058323ec3dc8fef2c91344095d6e75b2fbfcabb6949344243904ff206f2fcb99be506d040e1f3fade61  image.png

When there is a diff, uncomment the bellow to understand whats happening

$ cp image.png /tmp
$ cp diff.png /tmp
$ firefox /tmp/image.png
$ firefox /tmp/diff.png


  $ kill $GECKOPID
