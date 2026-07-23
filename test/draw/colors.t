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
#C90A0AFF  Polygon.fill alpha   Color.(rgb ~alpha:0.75 255 0 0) over the background
#0AC90AFF  degenerate fan alpha Color.(rgb ~alpha:0.75 0 255 0) over the background
(one flat color: any triangulation overlap would double-blend into extra colors)
#40E0D0FF  Circle.fill clipped  Color.turquoise

  $ if command -v magick > /dev/null; then IM=magick; else IM=convert; fi
  $ $IM jsoo.png txt:- | awk 'NR>1{print $3}' | sort | uniq -c | awk '$1 >= 2000 {print $2}' | sort > jsoo_colors
  $ $IM raylib.png txt:- | awk 'NR>1{print $3}' | sort | uniq -c | awk '$1 >= 2000 {print $2}' | sort > raylib_colors
  $ cat raylib_colors
  #0000FFFF
  #008080FF
  #044EADFF
  #0A2ACBFF
  #0AC332FF
  #0AC90AFF
  #18687EFF
  #282828FF
  #3296A3FF
  #40E0D0FF
  #41AA14FF
  #4B0082FF
  #7E4B18FF
  #A32A32FF
  #C90A0AFF
  #CB1E82FF
  #EE82EEFF
  #FF0000FF
  #FF00FFFF
  #FF7F50FF

The two backends produce the same colors, up to a least-significant bit on the
multi-layer alpha blends inside the clip regions: the raylib backend composites
each clipped draw through an 8-bit offscreen layer, one extra rounding step
versus the browser's direct blend, so such blends can land a step apart. A real
double-blend (an unintended extra layer) would be off by far more than 1 and so
would still be reported here. Every color must therefore match one on the other
backend within 1/255 on every channel, in both directions:

  $ within1() {
  >   awk 'BEGIN { s = "0123456789ABCDEF"; for (i = 1; i <= 16; i++) H[substr(s, i, 1)] = i - 1 }
  >     function hx(x, p) { return (H[substr(x, p, 1)] * 16) + H[substr(x, p + 1, 1)] }
  >     function near(a, b) { d = a - b; return (d < 0 ? -d : d) <= 1 }
  >     NR == FNR { ref[FNR] = $0; n = FNR; next }
  >     { for (i = 1; i <= n; i++)
  >         if (near(hx($0,2), hx(ref[i],2)) && near(hx($0,4), hx(ref[i],4)) \
  >          && near(hx($0,6), hx(ref[i],6)) && near(hx($0,8), hx(ref[i],8))) next
  >       print "unmatched: " $0 }' "$2" "$1"
  > }
  $ within1 raylib_colors jsoo_colors
  $ within1 jsoo_colors raylib_colors
