(lang dune 3.15)

(using directory-targets 0.1)

(generate_opam_files true)

(name gamelle)

(version 0.1)

(source
 (github art-w/gamelle))

(authors "Arthur Wendling" "Emile Trotignon")

(maintainers "art.wendling@gmail.com")

(license MIT)

(package
 (name gamelle)
 (synopsis "Gamelle")
 (depends
  (ocaml
   (>= 5.0.1))
  (js_of_ocaml
   (>= 5.6.0))
  (brr
   (>= 0.0.8))
  (gg
   (>= 1.0.0))
  (color
   (>= 0.3.0))
  (ctypes
   (>= 0.21.1))
  (result
   (>= 1.5))
  (crunch
   (>= 3.3.1))
  (ppx_blob
   (>= 0.7.2))
  (yojson
   (>= 2.0.0))
  (ppx_deriving_yojson
   (>= 3.7.0))
  (uuseg
   (>= 15.1.0))
  (otfm
   (>= 0.4.0))
  (imagelib
   (>= 20221222))
  (nanosvg
   (>= 0.2))
  (webdriver_cohttp-lwt-unix :with-test))
 (depopts
  (inotify
   (>= 2.1))
  (raylib
   (>= 2.2.1))
  (ctypes-foreign
   (>= 0.21.1))
  (lwt
   (>= 5.9.0))
  (uri
   (>= 4.0.0))
  (conduit-lwt-unix
   (>= 7.0.0))
  ; conduit-lwt-unix only speaks wss when a TLS backend is installed alongside
  ; it (it is a depopt of conduit); tls-lwt is the pure-OCaml one.
  (tls-lwt
   (>= 1.0.0))
  ; Core websocket frame codec only. The conduit/Lwt transport that
  ; websocket-lwt-unix would provide is rewritten
  ; because that package caps cohttp-lwt-unix < 6, which transitively caps
  ; cmdliner < 2 and so conflicts with wasm_of_ocaml in a shared switch.
  (websocket
   (>= 2.17))
  (base64
   (>= 3.0.0))))

(package
 (name gamelle-all)
 (synopsis "Gamelle with all optional deps")
 (depends
  gamelle
  raylib
  nanosvg
  ctypes
  ctypes-foreign
  inotify
  lwt
  uri
  conduit-lwt-unix
  tls-lwt
  websocket
  base64)
 (allow_empty))
