let src = Logs.Src.create "uniq.meta"

module Log = (val Logs.src_log src : Logs.LOG)

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let ( let* ) = Result.bind

type t =
  | Node of { name: string; value: string; contents: t list }
    (* [name] "[value]" ( [contents] ),
       like [package "lib" ( ... )] *)
  | Set of { name: string; predicates: predicate list; value: string }
    (* [name] [(...) as predicates] = [value],
       like [archive(native) = "lib.cmxa"]*)
  | Add of { name: string; predicates: predicate list; value: string }
(* [name] [(...) as predicates] = [value],
   like [archive(native) += "lib.cmxa"]*)

and predicate = Include of string | Exclude of string

let pp_predicate ppf = function
  | Include p -> Fmt.string ppf p
  | Exclude p -> Fmt.pf ppf "-%s" p

let rec pp ppf = function
  | Node { name; value; contents } ->
      Fmt.pf ppf "%s %S (@\n@[<2>%a@]@\n)" name value
        Fmt.(list ~sep:(any "@\n") pp)
        contents
  | Set { name; predicates= []; value } -> Fmt.pf ppf "%s = %S" name value
  | Set { name; predicates; value } ->
      Fmt.pf ppf "%s(%a) = %S" name
        Fmt.(list ~sep:(any ",") pp_predicate)
        predicates value
  | Add { name; predicates= []; value } -> Fmt.pf ppf "%s += %S" name value
  | Add { name; predicates; value } ->
      Fmt.pf ppf "%s(%a) += %S" name
        Fmt.(list ~sep:(any ",") pp_predicate)
        predicates value

module Assoc = struct
  type t = (string * string list) list

  let add k v t =
    match List.assoc_opt k t with
    | Some vs ->
        let vs = List.sort_uniq String.compare (v :: vs) in
        (k, vs) :: List.remove_assoc k t
    | None -> (k, [ v ]) :: t

  let set k v t =
    match List.assoc_opt k t with
    | Some _ -> (k, [ v ]) :: List.remove_assoc k t
    | None -> (k, [ v ]) :: t
end

module Path = struct
  type t = string list

  let of_string str =
    let pkg = String.split_on_char '.' str in
    let rec go = function
      | [] -> Ok pkg
      | "" :: _ -> error_msgf "Invalid package name: %S" str
      | _ :: rest -> go rest
    in
    go pkg

  let of_string_exn str =
    match of_string str with
    | Ok pkg -> pkg
    | Error (`Msg msg) -> invalid_arg msg

  let pp ppf pkg = Fmt.string ppf (String.concat "." pkg)
  let equal a b = try List.for_all2 String.equal a b with _ -> false
end

let incl ~predicates ps =
  let one = function
    | Include p -> List.exists (String.equal p) predicates
    | Exclude p -> not (List.exists (String.equal p) predicates)
  in
  List.exists one ps

let find_directory ~predicates contents =
  let rec go result = function
    | [] -> result
    | Add { name= "directory"; predicates= []; value } :: rest ->
        if Stdlib.Option.is_none result then go (Some value) rest
        else go result rest
    | Add { name= "directory"; predicates= ps; value } :: rest ->
        if incl ~predicates ps && Stdlib.Option.is_none result then
          go (Some value) rest
        else go result rest
    | Set { name= "directory"; predicates= []; value } :: rest ->
        go (Some value) rest
    | Set { name= "directory"; predicates= ps; value } :: rest ->
        if incl ~predicates ps then go (Some value) rest else go result rest
    | _ :: rest -> go result rest
  in
  go None contents

let compile ~predicates t ks =
  let rec go ~directory acc t = function
    | [] ->
        let rec go acc = function
          | [] ->
              let acc = List.remove_assoc "directory" acc in
              ("directory", [ directory ]) :: acc
          | Node _ :: rest -> go acc rest
          | Add { name; predicates= []; value } :: rest ->
              go (Assoc.add name value acc) rest
          | Set { name; predicates= []; value } :: rest ->
              go (Assoc.set name value acc) rest
          | Add { name; predicates= ps; value } :: rest ->
              if incl ~predicates ps then go (Assoc.add name value acc) rest
              else go acc rest
          | Set { name; predicates= ps; value } :: rest ->
              if incl ~predicates ps then go (Assoc.set name value acc) rest
              else go acc rest
        in
        go acc t
    | k :: ks -> (
        match t with
        | [] -> acc
        | Node { name= "package"; value; contents } :: rest ->
            let directory' =
              match find_directory ~predicates contents with
              | Some v -> Filename.concat directory v
              | None -> directory
            in
            if k = value then go ~directory:directory' acc contents ks
            else go ~directory acc rest (k :: ks)
        | _ :: rest -> go ~directory acc rest (k :: ks))
  in
  go ~directory:"" [] t ks

exception Parser_error of string

let raise_parser_error lexbuf fmt =
  let p = Lexing.lexeme_start_p lexbuf in
  let c = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  Fmt.kstr
    (fun msg -> raise (Parser_error msg))
    ("%s (l.%d c.%d): " ^^ fmt)
    p.Lexing.pos_fname p.Lexing.pos_lnum c

let pp_token ppf = function
  | Uniq_meta_lexer.Name name -> Fmt.string ppf name
  | String str -> Fmt.pf ppf "%S" str
  | Minus -> Fmt.string ppf "-"
  | Lparen -> Fmt.string ppf "("
  | Rparen -> Fmt.string ppf ")"
  | Comma -> Fmt.string ppf ","
  | Equal -> Fmt.string ppf "="
  | Plus_equal -> Fmt.string ppf "+="
  | Eof -> Fmt.string ppf "#eof"

let invalid_token lexbuf token =
  raise_parser_error lexbuf "Invalid token %a" pp_token token

let lparen lexbuf =
  match Uniq_meta_lexer.token lexbuf with
  | Lparen -> ()
  | token -> invalid_token lexbuf token

let name lexbuf =
  match Uniq_meta_lexer.token lexbuf with
  | Name name -> name
  | token -> invalid_token lexbuf token

let string lexbuf =
  match Uniq_meta_lexer.token lexbuf with
  | String str -> str
  | token -> invalid_token lexbuf token

let rec predicates lexbuf acc =
  match Uniq_meta_lexer.token lexbuf with
  | Rparen -> List.rev acc
  | Name predicate ->
      begin match Uniq_meta_lexer.token lexbuf with
      | Comma -> predicates lexbuf (Include predicate :: acc)
      | Rparen -> List.rev (Include predicate :: acc)
      | token -> invalid_token lexbuf token
      end
  | Minus ->
      let predicate = name lexbuf in
      begin match Uniq_meta_lexer.token lexbuf with
      | Comma -> predicates lexbuf (Exclude predicate :: acc)
      | Rparen -> List.rev (Exclude predicate :: acc)
      | token -> invalid_token lexbuf token
      end
  | token -> invalid_token lexbuf token

let rec parser lexbuf depth acc =
  match Uniq_meta_lexer.token lexbuf with
  | Rparen when depth > 0 -> List.rev acc
  | Rparen ->
      raise_parser_error lexbuf
        "Closing parenthesis without matching opening one"
  | Eof when depth = 0 -> List.rev acc
  | Eof -> raise_parser_error lexbuf "%d closing parenthesis missing" depth
  | Name name ->
      begin match Uniq_meta_lexer.token lexbuf with
      | String value ->
          lparen lexbuf;
          let contents = parser lexbuf (succ depth) [] in
          parser lexbuf depth (Node { name; value; contents } :: acc)
      | Equal ->
          let value = string lexbuf in
          parser lexbuf depth (Set { name; predicates= []; value } :: acc)
      | Plus_equal ->
          let value = string lexbuf in
          parser lexbuf depth (Add { name; predicates= []; value } :: acc)
      | Lparen ->
          let predicates = predicates lexbuf [] in
          begin match Uniq_meta_lexer.token lexbuf with
          | Equal ->
              let value = string lexbuf in
              parser lexbuf depth (Set { name; predicates; value } :: acc)
          | Plus_equal ->
              let value = string lexbuf in
              parser lexbuf depth (Add { name; predicates; value } :: acc)
          | token -> invalid_token lexbuf token
          end
      | token -> invalid_token lexbuf token
      end
  | token -> invalid_token lexbuf token

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let parser lexbuf =
  try Ok (parser lexbuf 0 []) with
  | Parser_error err -> Error (`Msg err)
  | Uniq_meta_lexer.Lexical_error (msg, f, l, c) ->
      error_msgf "%s at l.%d, c.%d: %s" f l c msg

let parser path =
  Log.debug (fun m -> m "Parse %a" Fpath.pp path);
  let ( let@ ) finally fn = Fun.protect ~finally fn in
  let ic = open_in (Fpath.to_string path) in
  let@ _ = fun () -> close_in ic in
  let lexbuf = Lexing.from_channel ic in
  Lexing.set_filename lexbuf (Fpath.to_string path);
  parser lexbuf

let rec incl us vs =
  match (us, vs) with
  | u :: us, v :: vs -> if u = v then incl us vs else false
  | [], _ | _, [] -> true

let rec diff us vs =
  match (us, vs) with
  | u :: us, v :: vs ->
      if u = v then diff us vs else error_msgf "Different paths (%S <> %S)" u v
  | [], x | x, [] -> Ok x

let relativize ~roots path =
  let rec go = function
    | [] -> assert false
    | root :: roots ->
        if Fpath.is_prefix root path then
          match Fpath.relativize ~root path with
          | Some rel -> (root, rel)
          | None -> go roots
        else go roots
  in
  go roots

let search ~roots ?(predicates = [ "native"; "byte" ]) meta_path =
  let ( >>= ) = Result.bind in
  let ( >>| ) x f = Result.map f x in
  let elements path =
    if Sys.is_directory (Fpath.to_string path) then Ok false
    else if Fpath.basename path = "META" then Ok true
    else Ok false
  in
  let traverse path =
    if List.exists (Fpath.equal path) roots then Ok true
    else begin
      let _, rel = relativize ~roots path in
      let meta_path' = List.filter (fun s -> s <> "") (Fpath.segs rel) in
      Ok (incl meta_path meta_path')
    end
  in
  let fold path acc =
    let root, rel = relativize ~roots path in
    let package = Fpath.(rem_empty_seg (parent rel)) in
    let meta_path' = Fpath.(segs package) in
    match
      diff meta_path meta_path' >>= fun ks ->
      parser path >>| fun meta -> compile ~predicates meta ks
    with
    | Ok descr -> Fpath.Map.add Fpath.(root // parent rel) descr acc
    | Error (`Msg msg) ->
        Log.warn (fun m ->
            m "Impossible to extract the META file of %a: %s" Fpath.pp path msg);
        acc
  in
  let err _path _ = Ok () in
  Bos.OS.Path.fold ~err ~dotfiles:false ~elements:(`Sat elements)
    ~traverse:(`Sat traverse) fold Fpath.Map.empty roots
  >>| Fpath.Map.bindings

let dependencies_of (_path, descr) =
  Stdlib.Option.value ~default:[] (List.assoc_opt "requires" descr)
  |> List.map (Astring.String.cuts ~empty:false ~sep:" ")
  |> List.concat
  |> List.map Path.of_string_exn

exception Cycle

let get_dependencies (_, path, descr) graph =
  let deps = dependencies_of (path, descr) in
  let fn name =
    match List.find_opt (fun (name', _, _) -> Path.equal name name') graph with
    | Some node -> [ node ]
    | None -> []
  in
  List.concat_map fn deps

type graph = (Path.t * Fpath.t * Assoc.t) list

let dfs (graph : graph) visited start =
  let rec explore path visited node =
    if List.mem node path then raise Cycle
    else if List.mem node visited then visited
    else
      let new_path = node :: path in
      let edges = get_dependencies node graph in
      let visited = List.fold_left (explore new_path) visited edges in
      node :: visited
  in
  explore [] visited start

(* topological sort *)

let sort graph =
  let fn visited node = dfs graph visited node in
  List.fold_left fn [] graph

let ancestors ~roots ?(predicates = [ "native"; "byte" ]) mpath =
  let rec go acc visited = function
    | [] -> Ok acc
    | mpath :: todo when List.mem mpath visited -> go acc visited todo
    | mpath :: todo ->
        begin match search ~roots ~predicates mpath with
        | Ok pkgs ->
            let requires = List.concat (List.map dependencies_of pkgs) in
            let fn (path, descr) = (mpath, path, descr) in
            let pkgs = List.map fn pkgs in
            go (List.rev_append pkgs acc) (mpath :: visited)
              (List.rev_append requires todo)
        | Error _ as err -> err
        end
  in
  let* lst = go [] [] [ mpath ] in
  Ok (sort lst |> List.rev)

let to_artifacts pkgs =
  let ( let* ) = Result.bind in
  let fn acc (path, pkg) =
    match acc with
    | Error _ as err -> err
    | Ok acc ->
        let directory = List.assoc_opt "directory" pkg in
        let* directory =
          match directory with
          | Some [ dir ] -> Ok Fpath.(path / dir)
          | Some _ ->
              error_msgf "Multiple directories referenced by %a" Fpath.pp
                Fpath.(path / "META")
          | None -> Ok path
        in
        let directory = Fpath.to_dir_path directory in
        let archive = List.assoc_opt "archive" pkg in
        let archive = Stdlib.Option.value ~default:[] archive in
        let plugin = List.assoc_opt "plugin" pkg in
        let plugin = Stdlib.Option.value ~default:[] plugin in
        let archive = List.map (Fpath.add_seg directory) archive in
        let plugin = List.map (Fpath.add_seg directory) plugin in
        Ok List.(rev_append archive (rev_append plugin acc))
  in
  let* paths = List.fold_left fn (Ok []) pkgs in
  Uniq_info.vs paths

let subpaths (meta : t list) : string list list =
  let rec go prefix acc = function
    | [] -> acc
    | Node { name= "package"; value; contents; _ } :: rest ->
        let path = prefix @ [ value ] in
        let sub = go path [] contents in
        go prefix ((path :: sub) @ acc) rest
    | _ :: rest -> go prefix acc rest
  in
  [] :: go [] [] meta

module MSet = Set.Make (Modname)

let submodules path =
  let cmi = Cmi_format.read_cmi path in
  let fn = function
    | Types.Sig_module (name, _, _, _, _) -> Some (Ident.name name)
    | _ -> None
  in
  match List.filter_map fn cmi.cmi_sign with
  | value -> value
  | exception _ -> []

(* Given a META-described (sub)package at [meta_dir] with descriptor [descr],
   return the directory where its .cmi files live. *)
let package_directory dname descr =
  match List.assoc_opt "directory" descr with
  | Some [ d ] when d <> "" ->
      begin match Fpath.of_string d with
      | Ok rel -> Fpath.(dname // rel |> to_dir_path)
      | Error _ -> Fpath.to_dir_path dname
      end
  | _ -> Fpath.to_dir_path dname

(* Register [full_ks] as a provider of [modname] in [acc] when [modname] is
   among the [targets] we are looking for. *)
let register_if_target targets full modname acc =
  if MSet.mem modname targets then
    let fn = function
      | None -> Some [ full ]
      | Some pkgs -> Some (full :: pkgs)
    in
    Modname.Map.update modname fn acc
  else acc

(* Scan the .cmi files inside [dir_str] and register every top-level module
   whose name belongs to [targets].  When [check_submodules] is true, also
   open each .cmi to discover sub-modules (e.g. Cmdliner exports Cmd, Term). *)
let scan_cmis ~and_submodules ~targets ~full ~dname acc =
  if Sys.file_exists dname && Sys.is_directory dname then
    let files = Sys.readdir dname in
    let fn acc fname =
      if not (Filename.check_suffix fname ".cmi") then acc
      else
        let base = Filename.chop_suffix fname ".cmi" in
        let modname = Modname.v (String.capitalize_ascii base) in
        let acc = register_if_target targets full modname acc in
        if and_submodules then
          let subs = submodules (Filename.concat dname fname) in
          let fn acc name =
            let m = Modname.v (String.capitalize_ascii name) in
            register_if_target targets full m acc
          in
          List.fold_left fn acc subs
        else acc
    in
    Array.fold_left fn acc files
  else acc

(* Walk every META file under [roots]; for each (sub)package, scan its .cmi
   directory and populate the module→packages map. *)
let walk_meta_files ~roots ~predicates ~and_submodules ~targets acc =
  let elements path =
    if Sys.is_directory (Fpath.to_string path) then Ok false
    else Ok (Fpath.basename path = "META")
  in
  let fn meta acc =
    let _, rel = relativize ~roots meta in
    let segs = Fpath.(segs (rem_empty_seg (parent rel))) in
    let base = List.filter (fun s -> s <> "") segs in
    match parser meta with
    | Error _ -> acc
    | Ok m ->
        let fn acc local =
          let full = base @ local in
          let descr = compile ~predicates m local in
          let dname =
            Fpath.to_string
              (package_directory Fpath.(parent meta |> rem_empty_seg) descr)
          in
          scan_cmis ~and_submodules ~targets ~full ~dname acc
        in
        List.fold_left fn acc (subpaths m)
  in
  let err _path _ = Ok () in
  Bos.OS.Path.fold ~err ~dotfiles:false ~elements:(`Sat elements) ~traverse:`Any
    fn acc roots
  |> Result.value ~default:acc

(* Deduplicate packages per module name. *)
let dedup result =
  let seen = Hashtbl.create 16 in
  let fn modname pkgs acc =
    let fn pkg =
      let key = String.concat "." pkg in
      match Hashtbl.find seen key with
      | _ -> false
      | exception Not_found -> Hashtbl.add seen key (); true
    in
    let uniques = List.filter fn (List.rev pkgs) in
    Hashtbl.reset seen; (modname, uniques) :: acc
  in
  Modname.Map.fold fn result [] |> List.rev

let find_providers ~roots ?(predicates = [ "native"; "byte" ]) modules =
  let targets = List.fold_left (fun s m -> MSet.add m s) MSet.empty modules in
  (* Pass 1: match by .cmi filename *)
  let result =
    walk_meta_files ~roots ~predicates ~and_submodules:false ~targets
      Modname.Map.empty
  in
  (* Pass 2: for unresolved modules, also check sub-module exports *)
  let resolved =
    Modname.Map.fold (fun m _ s -> MSet.add m s) result MSet.empty
  in
  let remaining = MSet.diff targets resolved in
  let result =
    if MSet.is_empty remaining then result
    else
      walk_meta_files ~roots ~predicates ~and_submodules:true ~targets:remaining
        result
  in
  dedup result

open Cmdliner

let directories =
  let doc = "The source directory containing the META files." in
  let parser str =
    match Fpath.of_string str with
    | Ok _ as v when Sys.file_exists str && Sys.is_directory str -> v
    | Ok v -> error_msgf "%a is not a directory or does not exist" Fpath.pp v
    | Error _ as err -> err
  in
  let open Arg in
  value
  & opt_all (conv (parser, Fpath.pp)) []
  & info [ "I" ] ~doc ~docv:"DIRECTORY"

let setup user's_directories =
  let cmd = Bos.Cmd.(v "ocamlfind" % "printconf" % "path") in
  let ( let* ) = Result.bind in
  let directories =
    let* exists = Bos.OS.Cmd.exists cmd in
    if exists then
      let r = Bos.OS.Cmd.run_out cmd in
      let* directories, _ = Bos.OS.Cmd.out_lines ~trim:true r in
      let directories =
        List.fold_left
          (fun acc path ->
            match Fpath.of_string path with
            | Ok path -> path :: acc
            | Error (`Msg _) ->
                Logs.warn (fun m ->
                    m "ocamlfind returned an invalid path: %S" path);
                acc)
          [] directories
      in
      Ok directories
    else Ok []
  in
  let directories = Result.value ~default:[] directories in
  List.rev_append directories user's_directories

let setup = Term.(const setup $ directories)
