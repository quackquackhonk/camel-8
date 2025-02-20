open Unix

type t = {
    delta_time: float;
    thresh: float;
    acc: float;
    last_update: float;
  }

let create fps ?(thresh = 0.0) = {
    delta_time = 1.0 /. fps;
    thresh = thresh;
    acc = 0.0;
    last_update = Unix.gettimeofday ()
  }

let update timer = timer

let advance _ = false
