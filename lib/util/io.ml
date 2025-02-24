let get_bytes fname =
  let s = In_channel.with_open_bin fname In_channel.input_all in
  Bytes.of_string s
