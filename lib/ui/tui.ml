open Emu

open Lwt
open LTerm_key
open LTerm_widget


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

let impl ~debug emu =
  (* Wrap the emulator in a ref *)
  let state = ref { emu = emu; key = None } in

  (* Setup *)
  let waiter, wakener = wait () in
  let main_box = new hbox in

  (* Screen objects*)
  let screen_label = new label (Display.to_string !state.emu.display) in
  let screen = new modal_frame in
  screen#set_label "Screen";
  screen#set screen_label;

  (* View of the emulator state*)
  let make_mem_view e = Memory.dump ~offset:8 e.ram e.cpu.pc in
  let ram_view_label = new label (make_mem_view !state.emu) in
  let ram_view = new frame in
  ram_view#set ram_view_label;
  let cpu_view_label = new label (Cpu.pretty !state.emu.cpu) in
  let cpu_view = new frame in
  cpu_view#set cpu_view_label;
  let emu_box = new vbox in
  emu_box#add cpu_view_label;
  emu_box#add ram_view_label;


  let emu_view = new frame in
  emu_view#set_label "Emulator State";
  emu_view#set emu_box;

  (* Add everything to the main box *)
  main_box#add screen;
  main_box#add emu_view;

  (* Update the emulator state every 1/60th of a second. *)
  let tick _ =
    state := { !state with emu = Emu.update ~key:!state.key !state.emu};
    screen_label#set_text @@ Display.to_string !state.emu.display;
    ram_view_label#set_text @@ make_mem_view !state.emu;
    cpu_view_label#set_text @@ Cpu.pretty !state.emu.cpu
  in
  ignore (Lwt_engine.on_timer (1.0 /. 60.0) true tick);

  (* Setup the keyboard handling for the hbox*)
  main_box#on_event (handler state (wakeup wakener));

  (* Run in the standard terminal. *)
  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
  Lwt.finalize
    (fun () -> run term main_box waiter)
    (fun () -> LTerm.disable_mouse term)

let run ?debug emu = Lwt_main.run (impl emu ~debug)
