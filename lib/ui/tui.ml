open Emu

open Lwt
open LTerm_geom
open LTerm_text
open LTerm_key

(* GLOBALS *)
type state = {
    emu: Emu.t;
    key: Uchar.t option
  }


let rec handler ui state =
  LTerm_ui.wait ui >>= function
    (* Exit on C-c *)
    | LTerm_event.Key { code = LTerm_key.Char c; control = true; _ } ->
       if c = Uchar.of_char 'c'
       then return ()
       else handler ui state
    (* Set key to any other pressed keys *)
    | LTerm_event.Key { code = LTerm_key.Char c; _ } ->
       handler ui state
    (* Don't do anything on other events *)
    | x -> handler ui state

let draw ui matrix state =
  let ui_size = LTerm_ui.size ui in
  let ctx = LTerm_draw.context matrix ui_size in
  LTerm_draw.clear ctx;
  LTerm_draw.draw_frame_labelled
    ctx
    { row1 = 0; col1 = 0; row2 = ui_size.rows; col2 = ui_size.cols }
    ~alignment:H_align_center
    (Zed_string.of_utf8 "weeee")
    LTerm_draw.Light;
  if ui_size.rows > 32 && ui_size.cols > 64 then begin
    let ctx = LTerm_draw.sub ctx { row1 = 1; col1 = 1; row2 = ui_size.rows - 1; col2 = ui_size.cols - 1 } in
    LTerm_draw.draw_styled ctx 0 0 (eval [B_fg LTerm_style.lblue; S (Display.to_string state.emu.display); E_fg])
  end


let impl ~debug emu =
  (* Setup *)

  (* Wrap the emulator in a ref *)
  let state = ref { emu = emu; key = None } in


  (* Run in the standard terminal. *)
  Lazy.force LTerm.stdout >>= fun term ->
  LTerm_ui.create term (fun ui matrix -> draw ui matrix !state)
  >>= fun ui ->

  (* Update the emulator state every 1/60th of a second. *)
  let tick _ = state := { !state with emu = Emu.update ~key:!state.key !state.emu};
               LTerm_ui.draw ui in
  ignore (Lwt_engine.on_timer (1.0 /. 60.0) true tick);
  Lwt.finalize (fun () -> handler ui state) (fun () -> LTerm_ui.quit ui)

let run ?debug emu = Lwt_main.run (impl emu ~debug)
