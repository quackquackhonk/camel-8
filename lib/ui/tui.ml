open Emu

open Lwt
open LTerm_geom
open LTerm_key
open LTerm_widget
open Stdint

(* GLOBALS *)
type state = {
    emu: Emu.t;
    key: Uchar.t option
  }

let handler state kill event = begin
    match event with
    (* Exit on C-c *)
    | LTerm_event.Key { code = LTerm_key.Char c; control = true; _ } ->
       if c = Uchar.of_char 'c'
       then kill()
       else state := { !state with key = Some c }
    (* Set key to any other pressed keys *)
    | LTerm_event.Key { code = LTerm_key.Char c; _ } ->
       state := { !state with key = Some c }
    (* Don't do anything on other events *)
    | x -> ()
  end; false

let impl emu =
  (* Wrap the emulator in a ref *)
  let state = ref { emu = emu; key = None } in
  (* Setup *)
  let waiter, wakener = wait () in
  let hbox = new hbox in

  (* Screen objects*)
  let screen_label = new label (Display.to_string !state.emu.display) in
  let screen = new frame in
  screen#set_label "Screen";
  screen#set screen_label;

  (* View of memory / PC objects*)
  let make_mem_view e = Memory.dump ~offset:8 e.ram e.cpu.pc in
  let ram_view_label = new label (make_mem_view !state.emu) in
  let ram_view = new frame in
  ram_view#set_label "Ram View";
  ram_view#set ram_view_label;

  (* Add everything to the hbox *)
  hbox#add screen;
  hbox#add ram_view;

  (* Update the emulator state every 1/60th of a second. *)
  let tick _ =
    state := { !state with emu = Emu.update ~key:!state.key !state.emu};
    screen_label#set_text @@ Display.to_string !state.emu.display;
    ram_view_label#set_text @@ make_mem_view !state.emu
  in
  ignore (Lwt_engine.on_timer (1.0 /. 60.0) true tick);

  (* Setup the keyboard handling for the hbox*)
  hbox#on_event (handler state (wakeup wakener));

  (* Run in the standard terminal. *)
  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
  Lwt.finalize
    (fun () -> run term hbox waiter)
    (fun () -> LTerm.disable_mouse term)

let run emu = Lwt_main.run (impl emu)
