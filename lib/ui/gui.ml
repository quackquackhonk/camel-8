module L = Bogue.Layout
module W = Bogue.Widget
open Tsdl
open Bogue
open Emu

let scale_factor = 10
let scale w h = (scale_factor * w, scale_factor * h)

(* Dimensions for the layout*)
let screen_w = 64
let screen_h = 32

let memory_w = 12
let memory_h = screen_h

let stack_w = 10
let stack_h = screen_h

let cpu_w = 24
let cpu_h = screen_h

let width = screen_w + memory_w + stack_w + cpu_w
let height = screen_h

type state = {
    emu: Emu.t;
    paused: bool;
  }

type area = {
    screen : L.t;
    memory : L.t;
    stack  : L.t;
    cpu    : L.t
  }

let thick_grey_line = Style.mk_line ~color:Draw.(opaque grey) ~width:3 ~style:Solid ()

let gray_border =
  let open Style in
  let border = mk_border thick_grey_line in
  let background = opaque_bg Draw.dark_grey in
  create ~background ~border ()

let draw_screen ~emu renderer =
  (* let w,h = Sdl_area.drawing_size area in *)
  let draw_pixel pixel x y =
    let color = Draw.(if pixel then opaque white else opaque dark_grey) in
    Draw.rectangle renderer ~color ~w:scale_factor ~h:scale_factor ~thick:scale_factor ~x ~y
  in
  let pixel_array = Display.to_bool_array emu.display in
  for row = 0 to 31 do
    for col = 0 to 63 do
      let pixel = pixel_array.(row).(col) in
      let px = col * scale_factor in
      let py = row * scale_factor in
      draw_pixel pixel px py
    done
  done

let init_area () =
  let screen = let w, h = scale screen_w screen_h in
               L.resident ~name:"screen" (W.sdl_area ~w ~h ()) in
  let memory = let w, h = scale memory_w memory_h in
               L.resident ~name:"memory" ~background:(Layout.opaque_bg @@ Draw.dark_grey) ~w ~h @@ W.verbatim "" in
  let stack = let w, h = scale stack_w stack_h in
              L.empty ~name:"stack" ~background:(Layout.opaque_bg @@ Draw.red) ~w ~h () in
  let cpu = let w, h = scale cpu_w cpu_h in
            L.empty ~name:"cpu" ~background:(Layout.opaque_bg @@ Draw.green) ~w ~h () in
  { screen = screen; memory = memory; stack = stack; cpu = cpu }

let init_state emu =
  { emu = emu; paused = false }

let update_state state =
  { state with emu = Emu.update state.emu }

let update_area area state =
  let a = W.get_sdl_area @@ L.widget area.screen in
  (* Clear the screen and redraw the screen*)
  Sdl_area.clear a;
  Sdl_area.add a (draw_screen ~emu:state.emu);

  let ram = L.widget area.memory in
  let ram_text = Memory.pretty ~offset:15 state.emu.ram state.emu.cpu.pc |> String.concat "\n" in
  W.set_text ram ram_text


let run ~debug emu =
  (* Create a reference for the application state  *)
  let state = init_state emu in
  let state = ref state in

  (* Initial window setup*)
  let area = init_area () in

  (* This is our main loop function *)
  let rec one_step () =
    (* Execute an instruction if we're not paused *)
    if not !state.paused
    then state := update_state !state ;
    (* Draw *)
    update_area area !state;
    Update.push (L.widget area.screen);
    Timeout.add (1000 / 60) one_step |> ignore
  in

  let layout =
    Layout.flat ~name:"Camel8"
      ~vmargin:scale_factor ~hmargin:scale_factor
      ~background:(Layout.opaque_bg @@ Draw.black)
      [area.screen; area.memory; area.stack; area.cpu]
  in

  let shortcuts = Bogue.shortcuts_empty () in
  (* Quit on C-c*)
  let shortcuts = Bogue.shortcuts_add_ctrl shortcuts Sdl.K.c (fun _ -> raise Bogue.Exit) in
  (* Toggle Pause on C-p*)
  let shortcuts =
    Bogue.shortcuts_add_ctrl shortcuts Sdl.K.p
      (fun _ -> state := { !state with paused = not !state.paused })
  in

  (* Create the board *)
  let board =  Bogue.of_layout ~shortcuts layout in

  (* Start the game loop*)
  Sync.push one_step;
  (* Run the board*)
  Bogue.run board
