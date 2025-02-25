open Emu

open Lwt
open LTerm_geom
open LTerm_text
open LTerm_key
open LTerm_widget
open Stdint

(* GLOBALS *)
let screen_size = { rows = 32; cols = 64 }

let leftmost_bit = Uint64.of_string "0x8000000000000000"

type state = {
    emu: Emu.t;
    key: Uchar.t option
  }

let rec old_handler ui emu =
  LTerm_ui.wait ui >>= function
  (* Quit on C-c *)
  | LTerm_event.Key { code = LTerm_key.Char c; control = true; _ } ->
     if c = Uchar.of_char 'c'
     then return ()
     else old_handler ui emu
  (* For now, send all keys to the update function*)
  | LTerm_event.Key { code = LTerm_key.Char c; _} ->
     emu := update ?key:(Some c) !emu;
     old_handler ui emu
  | x ->
     emu := update !emu;
     old_handler ui emu

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
  let screen_label = new label (Display.to_string !state.emu.display) in
  let screen = new modal_frame in
  let button = new button "exit" in
  screen#set screen_label;
  hbox#add screen;
  hbox#add button;

  (* Update the emulator state every 1/60th of a second. *)
  let tick _ =
    state := { !state with emu = Emu.update ~key:!state.key !state.emu};
    screen_label#set_text @@ Display.to_string !state.emu.display in

  ignore (Lwt_engine.on_timer (1.0 /. 60.0) true tick);

  (* Setup the keyboard handling for the hbox*)
  hbox#on_event (handler state (wakeup wakener));

  (* Quit when the exit button is clicked. *)
  button#on_click (wakeup wakener);

  (* Run in the standard terminal. *)
  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
  Lwt.finalize
    (fun () -> run term hbox waiter)
    (fun () -> LTerm.disable_mouse term)

let run emu = Lwt_main.run (impl emu)
