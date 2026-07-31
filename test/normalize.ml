(* It's a simple program which hides signatures and versions to be sure to work
   on any versions of OCaml. *)

let magic = "Version: "
let digest_length = 32

let is_hex = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let starts_with ~prefix str =
  let len = String.length prefix in
  String.length str >= len && String.sub str 0 len = prefix

let mask_digests line =
  let len = String.length line in
  let buf = Buffer.create len in
  let pos = ref 0 in
  while !pos < len do
    if is_hex line.[!pos] then begin
      let stop = ref !pos in
      while !stop < len && is_hex line.[!stop] do
        incr stop
      done;
      if !stop - !pos = digest_length then Buffer.add_string buf "<digest>"
      else Buffer.add_substring buf line !pos (!stop - !pos);
      pos := !stop
    end
    else begin
      Buffer.add_char buf line.[!pos];
      incr pos
    end
  done;
  Buffer.contents buf

let mask_magic line =
  if starts_with ~prefix:magic line then magic ^ "<version>" else line

let () =
  let me = Filename.basename Sys.executable_name in
  let rules = ref [] in
  let add rule = rules := rule :: !rules in
  let fn = function
    | "--digest" -> add mask_digests
    | "--magic" -> add mask_magic
    | arg ->
        Printf.eprintf "%s: invalid rule %S\n%!" me arg;
        exit 1
  in
  List.iter fn (List.tl (Array.to_list Sys.argv));
  if !rules = [] then begin
    Printf.eprintf "%s: at least one rule is required\n%!" me;
    exit 1
  end;
  let rules = List.rev !rules in
  let apply line = List.fold_left (fun line rule -> rule line) line rules in
  try
    while true do
      print_endline (apply (input_line stdin))
    done
  with End_of_file -> ()
