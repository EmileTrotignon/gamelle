Each filled draw call covers a large area; the thin strokes, labels and the
camel bitmap's edges contribute only a few hundred pixels each. Keeping only
colors above a 2000-pixel floor leaves exactly the solid fills, whose set is
stable across Mesa versions — unlike the near-threshold antialiasing colors,
which drift in and out per environment. Both backends must agree on that set:
#282828FF  background           Color.(rgb 40 40 40)
#0000FFFF  Box.fill             Color.blue
#FF0000FF  Circle.fill          Color.red
#FF00FFFF  Polygon.fill         Color.magenta
#008080FF  Box.fill (touch TL)  Color.teal
#FF7F50FF  Box.fill (touch TR)  Color.coral
#4B0082FF  Box.fill (touch BL)  Color.indigo
#EE82EEFF  Box.fill (touch BR)  Color.violet

  $ if command -v magick > /dev/null; then IM=magick; else IM=convert; fi
  $ $IM jsoo.png txt:- | awk 'NR>1{print $3}' | sort | uniq -c | awk '$1 >= 2000 {print $2}' | sort > jsoo_colors
  $ $IM raylib.png txt:- | awk 'NR>1{print $3}' | sort | uniq -c | awk '$1 >= 2000 {print $2}' | sort > raylib_colors
  $ cat raylib_colors
  #0000FFFF
  #008080FF
  #282828FF
  #4B0082FF
  #EE82EEFF
  #FF0000FF
  #FF00FFFF
  #FF7F50FF

The two backends produce the same set of solid colors:

  $ diff jsoo_colors raylib_colors
