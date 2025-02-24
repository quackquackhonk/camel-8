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

let rec handler ui emu =
  LTerm_ui.wait ui >>= function
  (* Quit on C-c *)
  | LTerm_event.Key { code = LTerm_key.Char c; control = true; _ } ->
     if c = Uchar.of_char 'c'
     then return ()
     else handler ui emu
  (* For now, send all keys to the update function*)
  | LTerm_event.Key { code = LTerm_key.Char c; _} ->
     emu := update ?key:(Some c) !emu;
     handler ui emu
  | x ->
     emu := update !emu;
     handler ui emu

let impl emu =
  (* Wrap the emulator in a ref *)
  let emu = ref emu in
  (* Setup *)
  let waiter, wakener = wait () in
  let hbox = new hbox in
  let screen = new label (Display.to_string !emu.display) in
  let button = new button "exit" in
  hbox#add screen;
  hbox#add button;

  (* Update the emulator state every 1/60th of a second. *)
  let tick _ =
    emu := Emu.update !emu;
    screen#set_text @@ Display.to_string !emu.display in
  ignore (Lwt_engine.on_timer 1.0 true tick);

  (* Run in the standard terminal. *)
  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
  Lwt.finalize
    (fun () -> run term hbox waiter)
    (fun () -> LTerm.disable_mouse term)

let run emu = Lwt_main.run (impl emu)
