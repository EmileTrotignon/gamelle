(* A small websocket client/server over conduit + Lwt, replacing the
   [websocket-lwt-unix] package. That package caps [cohttp-lwt-unix < 6], which
   transitively caps [cmdliner < 2] and so cannot share a switch with
   wasm_of_ocaml. The core [websocket] package (frame codec) has no such cap,
   so we drive it directly here.

   [websocket]'s codec is exposed only through [Websocket.Make (IO)] over a
   [Cohttp.S.IO]. cohttp 6 rebuilt that IO around its own buffered channel type,
   incompatible with the [Lwt_io] channels conduit hands us -- which is exactly
   why the upstream glue could not move to cohttp 6. The codec itself only ever
   calls [IO.read]/[IO.write] (see [read_exactly] in websocket.ml), so we supply
   a minimal [Cohttp.S.IO] backed by [Lwt_io] and perform the (trivial) HTTP
   Upgrade handshake by hand rather than through cohttp's request/response
   parsers. The unused [refill]/[with_input_buffer] members are stubs. *)

open Lwt.Infix

module Io = struct
  type +'a t = 'a Lwt.t

  let ( >>= ) = Lwt.bind
  let return = Lwt.return

  type ic = Lwt_io.input_channel
  type oc = Lwt_io.output_channel
  type conn = unit

  (* Only used by cohttp's message parsers, which we do not use. *)
  let refill _ic = Lwt.return `Ok

  let with_input_buffer _ic ~f =
    ignore f;
    failwith "gamelle_websocket: with_input_buffer is not used"

  let read_line ic = Lwt_io.read_line_opt ic
  let read ic len = Lwt_io.read ~count:len ic
  let write oc s = Lwt_io.write oc s
  let flush oc = Lwt_io.flush oc
end

module W = Websocket.Make (Io)

let set_tcp_nodelay flow =
  let open Conduit_lwt_unix in
  match flow with
  | TCP { fd; _ } -> Lwt_unix.setsockopt fd Lwt_unix.TCP_NODELAY true
  | _ -> ()

(* Read the CRLF-delimited status/request line and headers of an HTTP message,
   stopping at the blank line. [Lwt_io.read_line] strips the trailing [\n]; we
   also drop a trailing [\r] and lower-case header names. *)
let read_http_head ic =
  let strip_cr s =
    let n = String.length s in
    if n > 0 && s.[n - 1] = '\r' then String.sub s 0 (n - 1) else s
  in
  Lwt_io.read_line ic >>= fun first ->
  let rec loop acc =
    Lwt_io.read_line ic >>= fun line ->
    let line = strip_cr line in
    if line = "" then Lwt.return (List.rev acc)
    else
      match String.index_opt line ':' with
      | Some i ->
          let k =
            String.sub line 0 i |> String.trim |> String.lowercase_ascii
          in
          let v =
            String.sub line (i + 1) (String.length line - i - 1) |> String.trim
          in
          loop ((k, v) :: acc)
      | None -> loop acc
  in
  loop [] >>= fun headers -> Lwt.return (strip_cr first, headers)

exception Handshake_error of string

(* {1 Client} *)

type conn = {
  read_frame : unit -> Websocket.Frame.t Lwt.t;
  write_frame : Websocket.Frame.t -> unit Lwt.t;
  oc : Lwt_io.output_channel;
}

let read { read_frame; _ } = read_frame ()
let write { write_frame; _ } frame = write_frame frame
let close_transport { oc; _ } = Lwt_io.close oc

let frame_writer ~mode oc =
  let buf = Buffer.create 128 in
  fun frame ->
    Buffer.clear buf;
    W.write_frame_to_buf ~mode buf frame;
    Lwt.catch
      (fun () ->
        Lwt_io.write oc (Buffer.contents buf) >>= fun () -> Lwt_io.flush oc)
      (fun exn ->
        Lwt.async (fun () -> Lwt_io.close oc);
        Lwt.fail exn)

let connect ?(random_string = Websocket.Rng.init ())
    ?(ctx = Lazy.force Conduit_lwt_unix.default_ctx) client url =
  let nonce = Base64.encode_exn (random_string 16) in
  Conduit_lwt_unix.connect ~ctx client >>= fun (flow, ic, oc) ->
  set_tcp_nodelay flow;
  let host =
    match (Uri.host url, Uri.port url) with
    | Some h, Some p -> Printf.sprintf "%s:%d" h p
    | Some h, None -> h
    | None, _ -> ""
  in
  let path =
    let p = Uri.path_and_query url in
    if p = "" then "/" else p
  in
  let request =
    String.concat "\r\n"
      [
        Printf.sprintf "GET %s HTTP/1.1" path;
        Printf.sprintf "Host: %s" host;
        "Upgrade: websocket";
        "Connection: Upgrade";
        Printf.sprintf "Sec-WebSocket-Key: %s" nonce;
        "Sec-WebSocket-Version: 13";
        "";
        "";
      ]
  in
  Lwt.catch
    (fun () ->
      Lwt_io.write oc request >>= fun () ->
      Lwt_io.flush oc >>= fun () ->
      read_http_head ic >>= fun (status_line, headers) ->
      let ok_status =
        match String.split_on_char ' ' status_line with
        | _ :: code :: _ -> code = "101"
        | _ -> false
      in
      let ok_upgrade =
        match List.assoc_opt "upgrade" headers with
        | Some u -> String.lowercase_ascii u = "websocket"
        | None -> false
      in
      let ok_accept =
        match List.assoc_opt "sec-websocket-accept" headers with
        | Some a ->
            a = Websocket.b64_encoded_sha1sum (nonce ^ Websocket.websocket_uuid)
        | None -> false
      in
      if ok_status && ok_upgrade && ok_accept then Lwt.return_unit
      else Lwt.fail (Handshake_error ("bad server handshake: " ^ status_line)))
    (fun exn -> Lwt_io.close ic >>= fun () -> Lwt.fail exn)
  >>= fun () ->
  let mode = W.Client random_string in
  let read_frame = W.make_read_frame ~mode ic oc in
  let read_frame () =
    Lwt.catch read_frame (fun exn ->
        Lwt.async (fun () -> Lwt_io.close ic);
        Lwt.fail exn)
  in
  Lwt.return { read_frame; write_frame = frame_writer ~mode oc; oc }

(* {1 Server} *)

module Connected_client = struct
  type t = {
    recv : unit -> Websocket.Frame.t Lwt.t;
    send : Websocket.Frame.t -> unit Lwt.t;
    (* Serialise concurrent writes (the volley server broadcasts from a tick
       loop while also answering handshakes). *)
    write_mutex : Lwt_mutex.t;
  }

  let create ic oc =
    let mode = W.Server in
    {
      recv = W.make_read_frame ~mode ic oc;
      send = frame_writer ~mode oc;
      write_mutex = Lwt_mutex.create ();
    }

  let recv t = t.recv ()
  let send t frame = Lwt_mutex.with_lock t.write_mutex (fun () -> t.send frame)
end

let establish_server ?timeout ?stop
    ?(on_exn = fun exn -> !Lwt.async_exception_hook exn)
    ?(check_request = fun _headers -> true)
    ?(ctx = Lazy.force Conduit_lwt_unix.default_ctx) ~mode react =
  Conduit_lwt_unix.serve ~on_exn ?timeout ?stop ~ctx ~mode (fun flow ic oc ->
      set_tcp_nodelay flow;
      Lwt.catch
        (fun () ->
          read_http_head ic >>= fun (request_line, headers) ->
          let is_get =
            match String.split_on_char ' ' request_line with
            | meth :: _ -> String.uppercase_ascii meth = "GET"
            | [] -> false
          in
          let upgrade_ok =
            match List.assoc_opt "upgrade" headers with
            | Some u -> String.lowercase_ascii u = "websocket"
            | None -> false
          in
          match List.assoc_opt "sec-websocket-key" headers with
          | Some key when is_get && upgrade_ok && check_request headers ->
              let accept =
                Websocket.b64_encoded_sha1sum (key ^ Websocket.websocket_uuid)
              in
              let response =
                String.concat "\r\n"
                  [
                    "HTTP/1.1 101 Switching Protocols";
                    "Upgrade: websocket";
                    "Connection: Upgrade";
                    Printf.sprintf "Sec-WebSocket-Accept: %s" accept;
                    "";
                    "";
                  ]
              in
              Lwt_io.write oc response >>= fun () ->
              Lwt_io.flush oc >>= fun () ->
              react (Connected_client.create ic oc)
          | _ ->
              Lwt_io.write oc "HTTP/1.1 400 Bad Request\r\n\r\n" >>= fun () ->
              Lwt_io.flush oc)
        (function End_of_file -> Lwt.return_unit | exn -> Lwt.fail exn))
