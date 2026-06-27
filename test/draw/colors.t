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

  $ if command -v magick > /dev/null; then IM=magick; else IM=convert; fi
  $ $IM jsoo.png txt:- | awk 'NR>1{print $3}' | sort | uniq -c | sort -rn | awk '$1 >= 500' > jsoo_colors
  $ cat jsoo_colors
   321564 #282828FF
    64172 #0000FFFF
    16236 #FF7F50FF
    16236 #EE82EEFF
    16236 #4B0082FF
    16236 #008080FF
    11723 #FF0000FF
     7009 #FF00FFFF
      565 #C5A97FFF
      549 #CAAF83FF
      526 #2A2AD5FF

  $ $IM raylib.png txt:- | awk 'NR>1{print $3}' | sort | uniq -c | sort -rn | awk '$1 >= 500' > raylib_colors
  $ cat raylib_colors
   321169 #282828FF
    64172 #0000FFFF
    16236 #FF7F50FF
    16236 #EE82EEFF
    16236 #4B0082FF
    16236 #008080FF
    11596 #FF0000FF
     6990 #FF00FFFF
      650 #CAAF83FF
      610 #C5A97FFF

$ diff jsoo_colors raylib_colors

When there is a diff, uncomment the bellow to understand whats happening

$ cp image.png /tmp
$ cp diff.png /tmp
$ firefox /tmp/image.png
$ firefox /tmp/diff.png
