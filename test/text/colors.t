Count pixels per exact colour, in units of 1000, dropping any colour that rounds
to 0 (i.e. under 1000 px). The raw per-colour counts are not portable: the
browser draws text through the platform freetype, whose glyph-edge antialiasing
differs by Firefox build, so the exact counts jitter by tens of pixels between
environments and the near-threshold greys reshuffle entirely (that is what made
the old exact-histogram assertion fail in CI). Coarsening to /1000 and keeping
only the dominant colours absorbs that jitter while still catching a colour
going missing or wildly wrong. The raylib side is stb_truetype at whole-pixel
sizes, so it is deterministic; only the browser side moves.


  $ if command -v magick > /dev/null; then IM=magick; else IM=convert; fi
  $ $IM glyph_browser.png txt:- | awk 'NR>1{print $3}' | sort | uniq -c | sort -rn | awk '{c=int($1/1000); if (c>0) print c, $2}' > jsoo_colors
  $ cat jsoo_colors
  154 #FFFFFF
  8 #000000
  1 #FF7F7F

  $ $IM glyph_raylib.png txt:- | awk 'NR>1{print $3}' | sort | uniq -c | sort -rn | awk '{c=int($1/1000); if (c>0) print c, $2}' > raylib_colors
  $ cat raylib_colors
  154 #FFFFFFFF
  8 #000000FF
  1 #FF8080FF

$ diff jsoo_colors raylib_colors

  $ if command -v magick > /dev/null; then IM=magick; else IM=convert; fi
  $ $IM lines_browser.png txt:- | awk 'NR>1{print $3}' | sort | uniq -c | sort -rn | awk '{c=int($1/1000); if (c>0) print c, $2}' > jsoo_colors
  $ cat jsoo_colors
  380 #FFFFFFFF
  9 #FF7F7FFF
  2 #FF8080FF

  $ $IM lines_raylib.png txt:- | awk 'NR>1{print $3}' | sort | uniq -c | sort -rn | awk '{c=int($1/1000); if (c>0) print c, $2}' > raylib_colors
  $ cat raylib_colors
  378 #FFFFFFFF
  12 #FF8080FF

  $ if command -v magick > /dev/null; then IM=magick; else IM=convert; fi
  $ $IM roboto_glyph_browser.png txt:- | awk 'NR>1{print $3}' | sort | uniq -c | sort -rn | awk '{c=int($1/1000); if (c>0) print c, $2}' > jsoo_colors
  $ cat jsoo_colors
  146 #FFFFFF
  16 #000000
  1 #FF7F7F

  $ $IM roboto_glyph_raylib.png txt:- | awk 'NR>1{print $3}' | sort | uniq -c | sort -rn | awk '{c=int($1/1000); if (c>0) print c, $2}' > raylib_colors
  $ cat raylib_colors
  146 #FFFFFFFF
  16 #000000FF
  1 #FF8080FF

$ diff jsoo_colors raylib_colors

  $ $IM roboto_browser.png txt:- | awk 'NR>1{print $3}' | sort | uniq -c | sort -rn | awk '{c=int($1/1000); if (c>0) print c, $2}' > jsoo_colors
  $ cat jsoo_colors
  375 #FFFFFFFF
  8 #FF7F7FFF
  6 #000000FF
  2 #FF8080FF

  $ $IM roboto_raylib.png txt:- | awk 'NR>1{print $3}' | sort | uniq -c | sort -rn | awk '{c=int($1/1000); if (c>0) print c, $2}' > raylib_colors
  $ cat raylib_colors
  374 #FFFFFFFF
  10 #FF8080FF
  5 #000000FF

$ diff jsoo_colors raylib_colors

  $ $IM view_browser.png txt:- | awk 'NR>1{print $3}' | sort | uniq -c | sort -rn | awk '{c=int($1/1000); if (c>0) print c, $2}' > jsoo_colors
  $ cat jsoo_colors
  395 #FFFFFFFF
  1 #FF7F7FFF

  $ $IM view_raylib.png txt:- | awk 'NR>1{print $3}' | sort | uniq -c | sort -rn | awk '{c=int($1/1000); if (c>0) print c, $2}' > raylib_colors
  $ cat raylib_colors
  396 #FFFFFFFF
  1 #FF8080FF

When there is a diff, uncomment the bellow to understand whats happening

$ cp image.png /tmp
$ cp diff.png /tmp
$ firefox /tmp/image.png
$ firefox /tmp/diff.png
