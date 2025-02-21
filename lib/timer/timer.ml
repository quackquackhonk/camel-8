open Unix

type t = {
    fps: float;
    dt: float;
    fuzzy_dt: float;
    last_update: float;
  }

let create ?(thresh = 0.0) fps = {
    fps = fps;
    dt = 1.0 /. fps;
    fuzzy_dt = 1.0 /. (fps +. thresh);
    last_update = Unix.gettimeofday ()
  }

let update timer =
  { timer with last_update = Unix.gettimeofday () }

let advance timer =
  let current_time = Unix.gettimeofday () in
  let since_last_update = current_time -. timer.last_update in
  Int.of_float @@ floor since_last_update /. timer.dt
