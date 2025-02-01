type t = bytes

let init _ = Bytes.init 4096 (fun _ -> Char.chr 0)
