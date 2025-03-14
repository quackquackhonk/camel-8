module L = Bogue.Layout
module W = Bogue.Widget
open Bogue
open Emu

let scale = 10

let screen_w = 64
let screen_h = 32

let thick_grey_line =
  let open Bogue in
  Style.mk_line ~color:Draw.(opaque grey) ~width:3 ~style:Solid ()

let gray_border =
  let open Style in
  let border = mk_border thick_grey_line in
  create ~border ()

let draw_screen ~emu area renderer =
  (* let w,h = Sdl_area.drawing_size area in *)
  let draw_pixel pixel x y =
    let color = Draw.(if pixel then opaque black else opaque white) in
    Draw.rectangle renderer ~color ~w:scale ~h:scale
                         ~thick:scale ~x ~y in
  let pixel_array = Display.to_bool_array emu.display in
  for row = 0 to 31 do
    for col = 0 to 63 do
      let pixel = pixel_array.(row).(col) in
      let px = col * scale in
      let py = row * scale in
      draw_pixel pixel px py
    done
  done

let run ~debug emu =
  (* Wrap the emulator in a reference *)
  let emu = ref emu in
  let w = screen_w * scale in
  let h = screen_h * scale in
  let screen = W.sdl_area ~w ~h ~style:gray_border () in
  let screen_area = W.get_sdl_area screen in
  let layout = L.resident screen in
  let app =  Bogue.of_layout layout in
  let render _ = Sdl_area.add screen_area ~name:"screen" (draw_screen ~emu:!emu screen_area) in
  let update _ = emu := Emu.update !emu; render () in
  Bogue.run ~before_display:update app
