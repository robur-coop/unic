(* NOTE(dinosaure): The purpose of this module is to give priority to certain
   packages over others and to force the use of a specific package for a given
   module name. The configuration file should take the following form:

   {[
     module Cmd {
       use cmdliner
     }

     prefer digestif.c
   ]} *)

module Path = Uniq_meta.Path

type path = Uniq_meta.Path.t
type t = { prefers: Path.Set.t; overrides: path Modname.Map.t }

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let empty = { prefers= Path.Set.empty; overrides= Modname.Map.empty }

let use t modname pkg =
  { t with overrides= Modname.Map.add modname pkg t.overrides }

let prefer t pkg = { t with prefers= Path.Set.add pkg t.prefers }

let path =
  let dec = Path.of_string_exn and enc = Fmt.to_to_string Path.pp in
  Bcfgt.map ~dec ~enc Bcfgt.string

let modname =
  let dec = Modname.v and enc = Modname.to_string in
  Bcfgt.map ~dec ~enc Bcfgt.string

let prefers =
  let dec = Path.Set.of_list and enc = Path.Set.elements in
  let open Bcfgt in
  some (directive ~name:"prefer" Fun.id |> req ~pos:0 path Fun.id)
  |> map ~enc ~dec

let overrides =
  let dec = Modname.Map.of_list and enc = Modname.Map.to_list in
  let open Bcfgt in
  let m =
    directive ~name:"module" (fun modname use -> (modname, use))
    |> req ~pos:0 modname (fun (modname, _) -> modname)
    |> field "use" path (fun (_, path) -> path)
  in
  some m |> map ~enc ~dec

let load filepath =
  let ( let* ) = Result.bind in
  let* contents = Bos.OS.File.read filepath in
  let* cfg = Bcfg.parser (Lexing.from_string contents) in
  try
    let* prefers = Bcfgt.decode prefers cfg in
    let* overrides = Bcfgt.decode overrides cfg in
    Ok { prefers; overrides }
  with Invalid_argument msg -> error_msgf "%a: %s" Fpath.pp filepath msg

let disambiguate_with t modname candidates =
  match Modname.Map.find_opt modname t.overrides with
  | Some pkg -> List.find_opt (Path.equal pkg) candidates
  | None ->
      let fn pkg = List.exists (Path.equal pkg) candidates in
      List.find_opt fn (Path.Set.elements t.prefers)
