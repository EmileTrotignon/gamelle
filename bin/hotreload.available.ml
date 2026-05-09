let ui_replay ~io (nb_past, target_clock, nb_future) =
  let open Gamelle in
  let open Ui in
  let io = View.font_size 12 @@ View.color Color.white io in
  window ~io
    ~width:(fun _ -> Size.width (Window.size ~io))
    ~height:(fun h ->
      Gamelle_common.ui_replay_height := h;
      h)
  @@ fun [%ui] ->
  horizontal [%ui] @@ fun () ->
  let total_events = nb_past + nb_future in
  let present =
    if total_events <= 0 then 0
    else
      over [%ui] @@ fun () ->
      Ui.Custom.draw [%ui] (fun ~io box ->
          let module Box = Gamelle.Box in
          let size = Box.size box in
          let progress =
            8.0
            +. ((Size.width size -. 16.0) *. float nb_past /. float total_events)
          in
          let target =
            8.0
            +. (Size.width size -. 16.0)
               *. float target_clock /. float total_events
          in
          let missing =
            Box.v
              Vec.(Box.top_left box + Vec.v progress 0.0)
              (Size.v (target -. progress) (Size.height size))
          in
          if Box.width missing > 0.0 then
            Box.fill ~io ~color:Color.(with_alpha 0.2 red) missing);
      int_of_float
      @@ slider [%ui] ~min:0.0 ~max:(float total_events) (float target_clock)
  in
  let nb_future =
    reshape [%ui] ~width:(fun _ -> { flex = 0.0; min = 60.0; max = 60.0 })
    @@ fun () -> if button [%ui] "CLEAR" then 0 else nb_future
  in
  (nb_past, present, nb_future)

let to_string = function
  | Inotify.Access -> "Access"
  | Attrib -> "Attrib"
  | Close_write -> "Close_write"
  | Close_nowrite -> "Close_nowrite"
  | Create -> "Create"
  | Delete -> "Delete"
  | Delete_self -> "Delete_self"
  | Modify -> "Modify"
  | Move_self -> "Move_self"
  | Moved_from -> "Moved_from"
  | Moved_to -> "Moved_to"
  | Open -> "Open"
  | Ignored -> "Ignored"
  | Isdir -> "Isdir"
  | Q_overflow -> "Q_overflow"
  | Unmount -> "Unmount"

let () = ignore to_string
let wait () = Unix.sleepf 0.01

let[@inline never] mutex_protect m f =
  let open Mutex in
  lock m;
  match f () with
  | x ->
      unlock m;
      x
  | exception e ->
      (* NOTE: [unlock] does not poll for asynchronous exceptions *)
      unlock m;
      raise e

let watch ~lock cmxs_file =
  let th =
    Thread.create @@ fun () ->
    let previous_files = ref [] in
    let to_clean file = previous_files := file :: !previous_files in
    let cleanup () =
      let lst = !previous_files in
      previous_files := [];
      List.iter (fun file -> try Sys.remove file with _ -> to_clean file) lst
    in
    let target_file = Filename.concat (Sys.getcwd ()) cmxs_file in
    let count = ref 0 in
    while true do
      mutex_protect lock @@ fun () ->
      try
        let rec reload () =
          incr count;
          if Sys.file_exists target_file then
            let new_file =
              Filename.concat
                (Filename.dirname target_file)
                ("g" ^ string_of_int !count ^ "_"
                ^ Filename.basename target_file)
            in
            if Sys.file_exists new_file then reload ()
            else
              let copy_ok =
                Sys.command (Printf.sprintf "cp %S %S" target_file new_file)
              in
              if copy_ok = 0 then (
                cleanup ();
                to_clean new_file;
                try Dynlink.loadfile_private new_file
                with _err ->
                  (* Format.printf "dynlink failed: %s@." (Printexc.to_string _err); *)
                  (* Printexc.print_backtrace stdout; *)
                  wait ();
                  reload ())
              else (
                wait ();
                reload ())
        in
        reload ();
        let fd_watcher = Inotify.create () in
        Fun.protect ~finally:(fun () -> Unix.close fd_watcher) @@ fun () ->
        let watch = Inotify.add_watch fd_watcher target_file [ S_All ] in
        Fun.protect ~finally:(fun () -> Inotify.rm_watch fd_watcher watch)
        @@ fun () ->
        let _evs = Inotify.read fd_watcher in
        (* Format.printf "Events: @."; *)
        (* List.iter *)
        (*   (fun (_, kinds, _, _) -> *)
        (*     Format.printf "- "; *)
        (*     List.iter (fun e -> Format.printf "%s " (to_string e)) kinds; *)
        (*     Format.printf "@.") *)
        (*   _evs; *)
        (* Format.printf "@."; *)
        (* Inotify.rm_watch fd_watcher watch *)
        ()
      with
      | Unix.Unix_error (Unix.ENOENT, _, _) -> wait ()
      | err ->
          Format.printf "ERROR: %s@." (Printexc.to_string err);
          wait ()
    done
  in
  let _th = th () in
  ()

let run cmxs_file =
  Printexc.record_backtrace true;
  let lock = Mutex.create () in
  Mutex.lock lock;
  let init = ref true in
  watch ~lock cmxs_file;
  Gamelle.run (0, 0, 0) (fun ~io state ->
      if !init then (
        init := false;
        Mutex.unlock lock);
      ui_replay ~io state)
