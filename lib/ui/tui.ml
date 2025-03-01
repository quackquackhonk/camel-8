open Emu

open Lwt
open LTerm_geom
open LTerm_text
open LTerm_key

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

(* DRAWING CONSTANTS *)
let screen_size = { rows = 32; cols = 64; }
let screen_frame_size = { rows = screen_size.rows + 2; cols = screen_size.cols + 2; }
let memory_size = { rows = 32; cols = 17 }
let memory_frame_size = { rows = memory_size.rows + 2; cols = memory_size.cols + 2; }
let total_size = {
    rows = screen_frame_size.rows + memory_frame_size.rows;
    cols = screen_frame_size.cols + screen_frame_size.cols;
  }

(** [draw_screen ctx size state] draws the display in [state] onto the context [ctx] of [size].
    @raise Invalid_argument if [size] is not at least [screen_frame_size] (34 x 66) *)
let draw_screen ctx size state =
  (* Guard against screen size *)
  if size.rows < screen_frame_size.rows || size.cols < screen_frame_size.cols then
    raise (Invalid_argument "Context is not large enough to draw the screen!");
  (* draw the frame*)
  LTerm_draw.draw_frame_labelled
    ctx
    { row1 = 0; col1 = 0; row2 = screen_size.rows + 1; col2 = screen_size.cols + 1 }
    ~alignment:H_align_center
    (Zed_string.of_utf8 "Screen")
    LTerm_draw.Light;
  (* Shrink the context to the size of the screen *)
  let ctx = LTerm_draw.sub ctx { row1 = 1; col1 = 1; row2 = screen_size.rows; col2 = screen_size.cols } in
  let pxs = Display.to_bool_array state.emu.display in
  for r = 0 to 31 do
    for c = 0 to 63 do
      let pixel = if pxs.(r).(c) then "X" else "." in
      LTerm_draw.draw_styled ctx r c (eval [B_fg LTerm_style.lblue; S pixel; E_fg])
    done
  done

let draw_memory ctx size state =
  LTerm_draw.draw_frame_labelled
    ctx
    { row1 = 0; col1 = 0; row2 = memory_size.rows + 1; col2 = memory_size.cols + 1 }
    ~alignment:H_align_center
    (Zed_string.of_utf8 "Memory")
    LTerm_draw.Light;
  let ctx = LTerm_draw.sub ctx { row1 = 1; col1 = 1; row2 = memory_size.rows; col2 = memory_size.cols } in
  let lines = Memory.dump ~offset:15 state.emu.ram state.emu.cpu.pc in
  List.iteri (fun i line -> LTerm_draw.draw_styled ctx i 0 (eval [B_fg LTerm_style.lgreen; S line; E_fg])) lines

let draw_emu_state ctx size state = ()

let draw ui matrix state =
  let size = LTerm_ui.size ui in
  let ctx = LTerm_draw.context matrix size in
  if size.rows < total_size.rows || size.cols < total_size.cols then
    LTerm_draw.draw_styled ctx 0 0 (eval [B_fg LTerm_style.lblue; S "Screen size is too small!"; E_fg])
  else
    (* Check that we can draw the screen *)
    let screen_size = { rows = 32; cols = 64; } in
    let screen_frame_size = { rows = screen_size.rows + 2; cols = screen_size.cols + 2; } in
    (* Shrink the context to the size of the screen + the frame.
       TODO: might be worth making this initial position configurable *)
    let screen_ctx = LTerm_draw.sub ctx { row1 = 0; col1 = 0; row2 = screen_frame_size.rows; col2 = screen_frame_size.cols } in
    let memory_ctx = LTerm_draw.sub ctx {
                         row1 = 0;
                         col1 = screen_frame_size.cols;
                         row2 = memory_frame_size.rows;
                         col2 = screen_frame_size.cols + memory_frame_size.rows
                       }
    in
    LTerm_draw.clear ctx;
    draw_screen screen_ctx size state;
    draw_memory memory_ctx size state

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
