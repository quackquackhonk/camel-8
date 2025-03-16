open Emu

open Lwt
open LTerm_geom
open LTerm_text
open LTerm_key

type state = {
    emu: Emu.t;
    key: Hex.t option;
    paused: bool
  }

(*
 * INPUT HANDLING
 *)

module CharMap = Map.Make(Char)
(* 1234
   qwfp
   arst
   xcdv
 *)
let mappings =
  CharMap.empty
  |> CharMap.add '1' Hex.One
  |> CharMap.add '2' Hex.Two
  |> CharMap.add '3' Hex.Three
  |> CharMap.add '4' Hex.C
  |> CharMap.add 'q' Hex.Four
  |> CharMap.add 'w' Hex.Five
  |> CharMap.add 'f' Hex.Six
  |> CharMap.add 'p' Hex.D
  |> CharMap.add 'a' Hex.Seven
  |> CharMap.add 'r' Hex.Eight
  |> CharMap.add 's' Hex.Nine
  |> CharMap.add 't' Hex.E
  |> CharMap.add 'x' Hex.A
  |> CharMap.add 'c' Hex.Zero
  |> CharMap.add 'v' Hex.B
  |> CharMap.add 'd' Hex.F


let get_key_mapping c =
  let c = Uchar.to_char c in
  CharMap.find_opt c mappings

let set_key state c =
  let k = get_key_mapping c in
  state := { !state with key = k }

let rec handler ui state =
  LTerm_ui.wait ui >>= function
    (* Exit on C-c *)
  | LTerm_event.Key { code = LTerm_key.Char c; control = true; _ }
       when c = Uchar.of_char 'c' -> return ()
  | LTerm_event.Key { code = LTerm_key.Char c; control = true; _ }
       when c = Uchar.of_char 'p' ->
     state := { !state with paused = not !state.paused };
     handler ui state
  (* If paused, pressing n steps through the state. *)
  | LTerm_event.Key { code = LTerm_key.Char c; _ }
       when c = Uchar.of_char 'n' ->
     state := { !state with emu = Emu.update ~key:!state.key !state.emu };
     handler ui state
  (* Set key to any other pressed keys *)
  | LTerm_event.Key { code = LTerm_key.Char c; _ } ->
     set_key state c;
     handler ui state
  (* Don't do anything on other events *)
  | x -> handler ui state

(* DRAWING CONSTANTS
                                   1     1 1       1                  1
   1+----------------------------+ +-----+ +-------+------------------+
    |           64               | | 18  | |   8   |       32         |
    |                            | |     | |       |               10 |
    |                            | |     | |       |    registers     |
    |                            | |     | |       |                  |
    | 32      Screen             | | RAM | | Stack +------------------+1
    |                            | |     | |       |                  |
    |                            | |     | |       |      misc        |
    |                            | |     | |       |                21|
    |                            | |     | |       |                  |
    |                            | |     | |       |                  |
   1+----------------------------+ +-----+ +-------+------------------+
 *)

let frame_size s = { rows = s.rows + 2; cols = s.cols + 2}

let screen_size = { rows = 32; cols = 64; }
let screen_frame_size = frame_size screen_size

let memory_size = { rows = 32; cols = 18 }
let memory_frame_size = frame_size memory_size

let stack_size = { rows = 32; cols = 8 }
(* registers are binary formatted, so contents of the registers are 10 chars long

   Since its 2 per line, each row is line this
   ` A: 0bXXXXXXXX | A: 0bXXXXXXXX ` = 31 chars
 *)
let registers_size = { rows = 10; cols = 32 }
let cpu_size = {
    rows = 32;
    cols = stack_size.cols + 1 + registers_size.cols
  }
let cpu_frame_size = frame_size cpu_size

let total_size = {
    rows = screen_frame_size.rows;
    cols = screen_frame_size.cols + memory_frame_size.cols + cpu_frame_size.cols
  }

(*
 * HELPER FUNCTIONS
 *)

let inner_rect rect = {
    row1 = rect.row1 + 1; row2 = rect.row2 - 1;
    col1 = rect.col1 + 1; col2 = rect.col2 - 1
  }

let draw_frame ctx msg rect =
  let _ = LTerm_draw.draw_frame_labelled ctx rect ~alignment:H_align_center
            (Zed_string.of_utf8 msg) LTerm_draw.Light in
  LTerm_draw.sub ctx (inner_rect rect)

(*
 * RENDERING UTILS
 *)

(** [draw_screen ctx size state] draws the display in [state] onto the context [ctx] of [size].*)
let draw_screen ctx size state =
  (* Shrink the context to the size of the screen *)
  let screen_rect = { row1 = 0; col1 = 0; row2 = screen_size.rows + 1; col2 = screen_size.cols + 1 } in
  let ctx = draw_frame ctx "Screen" screen_rect in
  let pxs = Display.to_bool_array state.emu.display in
  for r = 0 to 31 do
    for c = 0 to 63 do
      let pixel = if pxs.(r).(c) then "█" else " " in
      LTerm_draw.draw_styled ctx r c (eval [B_fg LTerm_style.lblue; S pixel; E_fg])
    done
  done

(** [draw_memory ctx size state] draws the memory in [state] onto the context [ctx] of [size]. *)
let draw_memory ctx size state =
  let mem_rect = { row1 = 0; col1 = 0; row2 = memory_size.rows + 1; col2 = memory_size.cols + 1 } in
  let ctx = draw_frame ctx "RAM" mem_rect in
  let lines = Memory.pretty ~offset:15 state.emu.ram state.emu.cpu.pc in
  List.iteri (fun i line -> LTerm_draw.draw_styled ctx i 0 (eval [B_fg LTerm_style.lwhite; S line; E_fg])) lines


(** [draw_cpu ctx size state] draws the emulator cpu [state] on the [ctx] of [size].*)
let draw_cpu ctx size state =
  (* Draw the stack *)
  let stack_rect = { row1 = 0; col1 = 0; row2 = stack_size.rows + 1; col2 = stack_size.cols + 2 } in
  let stack_ctx = draw_frame ctx "Stack" stack_rect in
  (* pretty_stack returns the list reversed from how the
     stack actually is. i.e. the first string is the bottom of the stack.
     We do this so drawing from the bottom up is easy *)
  let lines = Cpu.pretty_stack ~max_addrs:(stack_size.rows - 1) state.emu.cpu in
  let draw_row i addr = let addr = " " ^ addr ^ " " in
                        let row = 30 - i in
                        LTerm_draw.draw_styled stack_ctx row 0 (eval [B_fg LTerm_style.lwhite; S addr; E_fg]);
  in
  List.iteri draw_row lines;
  (* Draw registers *)
  let reg_start_x = stack_size.cols + 2 in
  let reg_rect = { row1 = 0; row2 = cpu_size.rows + 1;
                   col1 = reg_start_x; col2 = registers_size.cols + reg_start_x + 2 } in
  let register_ctx = draw_frame ctx "Registers" reg_rect in
  let lines = Register.pretty state.emu.cpu.regs in
  let draw_reg i line = let start = if i < 8 then 1 else 16 in
                        let i = (i mod 8) + 1 in
                        LTerm_draw.draw_styled register_ctx i start (eval [B_fg LTerm_style.lwhite; S line; E_fg])
  in
  let _ = List.iteri draw_reg lines in
  (* Draw the Misc info *)
  let misc_start_y = registers_size.rows + 1 in
  let misc_rect = { row1 = misc_start_y; row2 = total_size.rows - 1;
                    col1 = reg_start_x; col2 = registers_size.cols + reg_start_x + 2 } in
  let misc_ctx = draw_frame ctx "Misc" misc_rect in
  let lines = [ Printf.sprintf "P: %s | %s"
                  (Pretty.uint16_to_bin_string state.emu.cpu.pc)
                  (Pretty.uint16_to_hex_string state.emu.cpu.pc);
                Printf.sprintf "I: %s | %s"
                  (Pretty.uint16_to_bin_string state.emu.cpu.index)
                  (Pretty.uint16_to_hex_string state.emu.cpu.index);
                Printf.sprintf "S: %s" (Pretty.uint8_to_bin_string state.emu.cpu.delay_timer);
                Printf.sprintf "D: %s" (Pretty.uint8_to_bin_string state.emu.cpu.sound_timer) ]
  in
  let draw_misc i line =
    LTerm_draw.draw_styled misc_ctx (i + 1) 1 (eval [B_fg LTerm_style.lwhite; S line; E_fg]) in
  let _ = List.iteri draw_misc lines in
  ()

let draw ui matrix state =
  let size = LTerm_ui.size ui in
  let ctx = LTerm_draw.context matrix size in
  let _ = LTerm_draw.clear ctx in
  if size.rows < total_size.rows || size.cols < total_size.cols then
    LTerm_draw.draw_styled ctx 0 0 (eval [B_fg LTerm_style.lblue; S "Screen size is too small!"; E_fg])
  else
    (* Shrink the context to the size of each component. *)
    let screen_ctx = LTerm_draw.sub ctx { row1 = 0; col1 = 0; row2 = screen_frame_size.rows; col2 = screen_frame_size.cols } in
    let memory_ctx = LTerm_draw.sub ctx {
                         row1 = 0; row2 = total_size.rows;
                         col1 = screen_frame_size.cols - 1 ;
                         col2 = screen_frame_size.cols + memory_frame_size.rows
                       }
    in
    let cpu_ctx = LTerm_draw.sub ctx {
                      row1 = 0; row2 = total_size.rows;
                      col1 = screen_frame_size.cols + memory_frame_size.cols - 2;
                      col2 = screen_frame_size.cols + 1 + memory_frame_size.cols + 1 + cpu_frame_size.cols
                    }
    in
    draw_screen screen_ctx size state;
    draw_memory memory_ctx size state;
    draw_cpu cpu_ctx size state

let impl ~debug emu =
  (* Wrap the emulator in a ref *)
  let state = ref { emu = emu; key = None; paused = true } in

  (* Run in the standard terminal. *)
  Lazy.force LTerm.stdout >>= fun term ->
  LTerm_ui.create term (fun ui matrix -> draw ui matrix !state)
  >>= fun ui ->

  (* Update the emulator state every 1/60th of a second. *)
  let tick _ =
    if not !state.paused
    then state := { !state with emu = Emu.update ~key:!state.key !state.emu};
    LTerm_ui.draw ui
  in
  ignore (Lwt_engine.on_timer (1.0 /. 60.0) true tick);

  (* Actually start the app *)
  try Lwt.finalize (fun () -> handler ui state) (fun () -> LTerm_ui.quit ui)
  with Failure _ -> LTerm_ui.quit ui

let run ?debug emu = Lwt_main.run (impl emu ~debug)
