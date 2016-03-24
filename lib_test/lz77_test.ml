let walk directory pattern =
  let select str = Re_str.string_match (Re_str.regexp pattern) str 0 in
  let rec aux acc = function
    | [] -> acc
    | dir :: rest ->
      let contents = Array.to_list (Sys.readdir dir) in
      let contents = List.rev_map (Filename.concat dir) contents in
      let dirs, files =
        List.fold_left (fun (dirs, files) kind ->
          match (Unix.stat kind).Unix.st_kind with
          | Unix.S_REG -> (dirs, kind :: files)
          | Unix.S_DIR -> (kind :: dirs, files)
          | _ -> (dirs, files))
          ([], []) contents
      in
      let matched = List.filter select files in
      aux (matched @ acc) (dirs @ rest)
  in
  aux [] [directory]

let load_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  s |> Bytes.to_string

let files directory = walk directory ".*\\.ml"

let generate length =
  let gen () = match Random.int (26 + 26 + 10) with
    | n when n < 26 -> int_of_char 'a' + n
    | n when n < 26 + 26 -> int_of_char 'A' + n - 26
    | n -> int_of_char '0' + n - 26 - 26 in
  let gen _ = String.make 1 (char_of_int (gen ())) in
  String.concat "" (Array.to_list (Array.init length gen))

let () = Random.self_init ()
let () = Printexc.record_backtrace true

module Byte =
struct
  type t = char

  let to_int = Char.code
end

module Bytes =
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

module Lz77 = Lz77.Make(Byte)(Bytes)

let make_string_test ?(level = 3) size =
  let data = generate size in
  Printf.sprintf "LZ77 (level = %d) for a random string" level,
  `Slow,
  (fun () -> Alcotest.(check string) data
    (Lz77.compress data 0 size |> Lz77.decompress) data)

let make_file_test ?(level = 3) filename =
  let data = load_file filename in
  let size = String.length data in
  Printf.sprintf "LZ77 (level = %d) for %s" level filename,
  `Slow,
  (fun () ->
    Alcotest.(check string) data
      (Lz77.compress data 0 size |> Lz77.decompress) data)

let test_strings number =
  Array.init number (fun _ -> make_string_test (0xFF + Random.int 0xFFFF))
  |> Array.to_list

let test_files directory =
  files directory
  |> List.map make_file_test

let () =
  Alcotest.run "Decompress test"
    [ "string", test_strings 25
    ; "file", test_files "./lib_test/files/"]
