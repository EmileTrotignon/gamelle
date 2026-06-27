Count pixels per exact color, filtering out colors with fewer than 500 pixels
(anti-aliasing noise).


  $ if command -v magick > /dev/null; then IM=magick; else IM=convert; fi
  $ $IM glyph_browser.png txt:- | awk 'NR>1{print $3}' | sort | uniq -c | sort -rn | awk '$1 >= 250' > jsoo_colors
  $ cat jsoo_colors
   154594 #FFFFFF
     8558 #000000
     1153 #FF7F7F
      444 #1B1B1B
      296 #C3C3C3
      290 #FFB5B5
      289 #FF4A4A
      254 #676767

  $ $IM glyph_raylib.png txt:- | awk 'NR>1{print $3}' | sort | uniq -c | sort -rn | awk '$1 >= 250' > raylib_colors
  $ cat raylib_colors
   154372 #FFFFFFFF
     8550 #000000FF
     1150 #FF7F7FFF
      289 #FFCBCBFF
      289 #FF3434FF

$ diff jsoo_colors raylib_colors

  $ if command -v magick > /dev/null; then IM=magick; else IM=convert; fi
  $ $IM lines_browser.png txt:- | awk 'NR>1{print $3}' | sort | uniq -c | sort -rn | awk '$1 >= 250' > jsoo_colors
  $ cat jsoo_colors
   380002 #FFFFFFFF
     9852 #FF7F7FFF
     2943 #FF8080FF
      719 #000000FF
      288 #232323FF
      265 #A7A7A7FF
      259 #838383FF

  $ $IM lines_raylib.png txt:- | awk 'NR>1{print $3}' | sort | uniq -c | sort -rn | awk '$1 >= 250' > raylib_colors
  $ cat raylib_colors
   378836 #FFFFFFFF
    12720 #FF7F7FFF
      499 #000000FF
      275 #FEFEFEFF
      270 #838383FF

  $ if command -v magick > /dev/null; then IM=magick; else IM=convert; fi
  $ $IM roboto_glyph_browser.png txt:- | awk 'NR>1{print $3}' | sort | uniq -c | sort -rn | awk '$1 >= 250' > jsoo_colors
  $ cat jsoo_colors
   146662 #FFFFFF
    16308 #000000
     1379 #FF7F7F
      340 #CFCFCF
      316 #9F9F9F
      282 #FFEEEE
      280 #FF1111
      271 #FF8080

  $ $IM roboto_glyph_raylib.png txt:- | awk 'NR>1{print $3}' | sort | uniq -c | sort -rn | awk '$1 >= 250' > raylib_colors
  $ cat raylib_colors
   146386 #FFFFFFFF
    16264 #000000FF
     1644 #FF7F7FFF
      340 #C9C9C9FF
      282 #FFFBFBFF
      280 #FF0404FF

$ diff jsoo_colors raylib_colors

  $ $IM roboto_browser.png txt:- | awk 'NR>1{print $3}' | sort | uniq -c | sort -rn | awk '$1 >= 250' > jsoo_colors
  $ cat jsoo_colors
   375270 #FFFFFFFF
     8254 #FF7F7FFF
     6496 #000000FF
     2494 #FF8080FF
      387 #5B5B5BFF
      330 #DBDBDBFF
      316 #EFEFEFFF
      258 #7F7F7FFF
      253 #575757FF

  $ $IM roboto_raylib.png txt:- | awk 'NR>1{print $3}' | sort | uniq -c | sort -rn | awk '$1 >= 250' > raylib_colors
  $ cat raylib_colors
   374731 #FFFFFFFF
    10627 #FF7F7FFF
     5915 #000000FF
      318 #010101FF
      305 #5A5A5AFF
      255 #DFDFDFFF

$ diff jsoo_colors raylib_colors

When there is a diff, uncomment the bellow to understand whats happening

$ cp image.png /tmp
$ cp diff.png /tmp
$ firefox /tmp/image.png
$ firefox /tmp/diff.png
