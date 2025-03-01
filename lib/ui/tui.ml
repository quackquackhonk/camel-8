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

(* DRAWING CONSTANTS
    The layout is as follows:
      17  1  8    1      ????
    +-----+-------+-----------------+
    |     |       |                 |
    |     |       |  Registers      |  ???
    |     |       |                 |
    |     |       |                 |
 32 | RAM | Stack +-----------------+ 1
    |     |       |                 |
    |     |       |                 |
    |     |       |   Other CPU     |  ???
    |     |       |     info        |
    |     |       |                 |
    +-----+-------+-----------------+
    ???


                                   1     1 1       1                  1
   1+----------------------------+ +-----+ +-------+------------------+
    |           64               | | 17  | |   8   |       31         |
    |                            | |     | |       |               10 |
    |                            | |     | |       |    registers     |
    |                            | |     | |       |                  |
    | 32      Screen             | | RAM | | Stack +------------------+1
    |                            | |     | |       |                  |
    |                            | |     | |       |                  |
    |                            | |     | |       |    misc info   21|
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
let registers_size = { rows = 10; cols = 31 }
let cpu_size = {
    rows = 32;
    cols = stack_size.cols + 1 + registers_size.cols
  }
let cpu_frame_size = frame_size cpu_size

let total_size = {
    rows = screen_frame_size.rows;
    cols = screen_frame_size.cols + memory_frame_size.cols + cpu_frame_size.cols
  }

(** [draw_screen ctx size state] draws the display in [state] onto the context [ctx] of [size].
    @raise Invalid_argument if [size] is not at least [screen_frame_size] (34 x 66) *)
let draw_screen ctx size state =
  (* Guard against screen size *)
  if size.rows < screen_frame_size.rows || size.cols < screen_frame_size.cols then
    raise (Invalid_argument "Context is not large enough to draw the screen!");
  (* draw the frame *)
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
    (Zed_string.of_utf8 "RAM")
    LTerm_draw.Light;
  let ctx = LTerm_draw.sub ctx { row1 = 1; col1 = 1; row2 = memory_size.rows; col2 = memory_size.cols } in
  let lines = Memory.pretty ~offset:15 state.emu.ram state.emu.cpu.pc in
  List.iteri (fun i line -> LTerm_draw.draw_styled ctx i 0 (eval [B_fg LTerm_style.lwhite; S line; E_fg])) lines


(** [draw_cpu ctx size state] draws the emulator cpu [state] on the [ctx] of [size].*)
let draw_cpu ctx size state =
  (* Draw stack frame *)
  LTerm_draw.draw_frame_labelled
    ctx
    { row1 = 0; col1 = 0; row2 = stack_size.rows + 1; col2 = stack_size.cols + 2 }
    ~alignment:H_align_center
    (Zed_string.of_utf8 "Stack")
    LTerm_draw.Light;
  (* Draw register frame*)
  let reg_start_x = stack_size.cols + 2 in
  LTerm_draw.draw_frame_labelled
    ctx
    { row1 = 0; col1 = reg_start_x; row2 = cpu_size.rows + 1; col2 = registers_size.cols + reg_start_x + 2 }
    ~alignment:H_align_center
    (Zed_string.of_utf8 "Registers")
    LTerm_draw.Light;
  let misc_start_y = stack_size.cols + 2 in
  LTerm_draw.draw_frame_labelled
    ctx
    { row1 = misc_start_y; col1 = reg_start_x; row2 = cpu_size.rows + 1; col2 = registers_size.cols + reg_start_x + 2 }
    ~alignment:H_align_center
    (Zed_string.of_utf8 "Misc")
    LTerm_draw.Light;
  (* Draw misc frame *)
  let inner_ctx = LTerm_draw.sub ctx { row1 = 1; col1 = 1; row2 = cpu_size.rows; col2 = cpu_size.cols } in
  let stack_ctx = LTerm_draw.sub inner_ctx { row1 = 0; col1 = 0; row2 = cpu_size.rows - 1; col2 = stack_size.cols } in
  let register_ctx = LTerm_draw.sub inner_ctx { row1 = 0; col1 = stack_size.cols; row2 = registers_size.rows; col2 = cpu_size.cols - 1 } in
  (* Draws boundaries between the segmets *)
  (* Draw the stack *)
  (* pretty_stack returns the list reversed from how the
     stack actually is. i.e. the first string is the bottom of the stack.
     We do this so drawing from the bottom up is easy *)
  let lines = Cpu.pretty_stack ~max_addrs:(stack_size.rows - 1) state.emu.cpu in
  let draw_row i addr = let addr = " " ^ addr ^ " " in
                        let row = 30 - i in
                        LTerm_draw.draw_styled stack_ctx row 0 (eval [B_fg LTerm_style.lwhite; S addr; E_fg]);
  in
  List.iteri draw_row lines;
  (* Draw the registers*)
  let lines = Register.pretty state.emu.cpu.regs in
  let draw_reg i line = let start = if i < 8 then 0 else 15 in
                        let i = i mod 8 in
                        LTerm_draw.draw_styled register_ctx i (start + 3) (eval [B_fg LTerm_style.lwhite; S line; E_fg])
  in
  List.iteri draw_reg lines;
  ()

let draw ui matrix state =
  let size = LTerm_ui.size ui in
  let ctx = LTerm_draw.context matrix size in
  if size.rows < total_size.rows || size.cols < total_size.cols then
    LTerm_draw.draw_styled ctx 0 0 (eval [B_fg LTerm_style.lblue; S "Screen size is too small!"; E_fg])
  else
    (* Shrink the context to the size of the screen + the frame. *)
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
    LTerm_draw.clear ctx;
    draw_screen screen_ctx size state;
    draw_memory memory_ctx size state;
    draw_cpu cpu_ctx size state


let impl ~debug emu =
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
