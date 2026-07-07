open Brr

(* The browser WebSocket API is event-based: [onmessage] fires on the JS event
   loop between animation frames. We just buffer incoming text frames in a
   queue that [poll] drains; no Lwt or promises are involved.

   Every failure is surfaced through [status]: an invalid url, a failed
   connection, an abnormal close or an unexpected binary frame all end up as
   [Error _]. Nothing is silently dropped — in particular [send] raises when
   the socket is not open instead of discarding the message. *)

type status = Connecting | Connected | Closed | Error of string

type t = {
  ws : Brr_io.Websocket.t option;
      (* [None] when the socket could not even be created (invalid url); in
         that case [error] is always set. *)
  recv : string Queue.t;
  mutable error : string option;
  (* Distinguishes a close we asked for from the server or network dying: an
     abnormal close event after [close] is not an error. *)
  mutable closed_by_user : bool;
}

let status_to_string = function
  | Connecting -> "connecting"
  | Connected -> "connected"
  | Closed -> "closed"
  | Error msg -> "error: " ^ msg

let set_error t msg = if t.error = None then t.error <- Some msg

(* The browser's [error] event carries no detail (for security reasons); the
   [close] event that follows carries a code and sometimes a reason, so let it
   replace this placeholder message. *)
let generic_error = "websocket error"

let connect url =
  let recv = Queue.create () in
  match Brr_io.Websocket.create (Jstr.of_string url) with
  | exception Jv.Error e ->
      (* An invalid url throws synchronously; report it through [status] like
         every other connection failure instead of raising mid-frame. *)
      {
        ws = None;
        recv;
        error = Some (Jstr.to_string (Jv.Error.message e));
        closed_by_user = false;
      }
  | ws ->
      let t = { ws = Some ws; recv; error = None; closed_by_user = false } in
      let target = Brr_io.Websocket.as_target ws in
      let _message : Ev.listener =
        Ev.listen Brr_io.Message.Ev.message
          (fun ev ->
            let data : Jv.t = Brr_io.Message.Ev.data (Ev.as_type ev) in
            if Jstr.equal (Jv.typeof data) (Jstr.v "string") then
              Queue.add (Jv.to_string data) t.recv
            else
              (* Binary frames are not part of [Net]'s protocol; fail rather
                 than deliver mangled data. *)
              set_error t "received a non-text websocket frame")
          target
      in
      let _error : Ev.listener =
        Ev.listen Ev.error (fun _ev -> set_error t generic_error) target
      in
      let _close : Ev.listener =
        Ev.listen Brr_io.Websocket.Ev.close
          (fun ev ->
            let e = Ev.as_type ev in
            let module Close = Brr_io.Websocket.Ev.Close in
            if (not (Close.was_clean e)) && not t.closed_by_user then begin
              let reason = Jstr.to_string (Close.reason e) in
              let msg =
                Printf.sprintf
                  "connection failed or closed abnormally (code %d%s)"
                  (Close.code e)
                  (if reason = "" then "" else ": " ^ reason)
              in
              (* Upgrade the detail-less [error] event message. *)
              if t.error = None || t.error = Some generic_error then
                t.error <- Some msg
            end)
          target
      in
      t

let status t =
  match (t.error, t.ws) with
  | Some msg, _ -> Error msg
  | None, None -> assert false (* [error] is always set when [ws] is [None] *)
  | None, Some ws ->
      let st = Brr_io.Websocket.ready_state ws in
      let open Brr_io.Websocket.Ready_state in
      if st = connecting then Connecting
      else if st = open' then Connected
      else Closed (* closing or closed *)

let send t msg =
  match status t with
  | Connected ->
      Brr_io.Websocket.send_string (Option.get t.ws) (Jstr.of_string msg)
  | (Connecting | Closed | Error _) as st ->
      failwith
        (Printf.sprintf
           "Net.send: the connection is not open (%s); check Net.status before \
            sending"
           (status_to_string st))

let poll t =
  let rec drain acc =
    if Queue.is_empty t.recv then List.rev acc
    else drain (Queue.pop t.recv :: acc)
  in
  drain []

let is_connected t = status t = Connected

let close t =
  t.closed_by_user <- true;
  match t.ws with Some ws -> Brr_io.Websocket.close ws | None -> ()
