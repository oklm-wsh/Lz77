let rec unix_read fd s j l =
  try Unix.read fd s j l
  with Unix.Unix_error (Unix.EINTR, _, _) -> unix_read fd s j l

let string_of_channel ?(use_unix = true) ic =
  let b = Buffer.create 65536 in
  let input, s =
    if use_unix
    then unix_read (Unix.descr_of_in_channel ic), Bytes.create 65536
    else input ic, Bytes.create 65536
  in
  let rec loop b input s =
    let rc = input s 0 (String.length s) in
    if rc = 0
    then Buffer.contents b
    else (Buffer.add_substring b s 0 rc; loop b input s)
  in
  loop b input s |> Bytes.to_string

let () = Printexc.record_backtrace true

module Atom =
struct
  type t = char

  let to_int = Char.code
end

module Scalar =
struct
  type elt = char
  type t = Bytes.t

  let create = Bytes.create
  let length = Bytes.length
  let get    = Bytes.get
  let set    = Bytes.set
  let blit   = Bytes.blit
  let sub    = Bytes.sub
end

module Lz77 = Lz77.Make(Atom)(Scalar)

let () =
  let buff = string_of_channel stdin in
  (Lz77.compress ~level:3 buff 0 (String.length buff)
   |> Lz77.decompress) = buff
  |> Printf.eprintf "%b\n%!"
