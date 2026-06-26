let src = Logs.Src.create "uniq.solver"

module Log = (val Logs.src_log src : Logs.LOG)
module MSet = Set.Make (Modname)
module Info = Uniq_info
module Digest = Uniq_digest

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let somef fmt = Fmt.kstr Stdlib.Option.some fmt

type cfg = {
    stdlib: bool
  ; recurse: bool
  ; exclude: Fpath.t list
  ; ignore: MSet.t
  ; forbid: MSet.t
}

type private_module = Modname.t * Uniq_digest.t option

let config ?(stdlib = true) ?(recurse = true) ?(exclude = []) ?(ignore = [])
    ?(forbid = []) () =
  let ignore = MSet.of_list ignore in
  let forbid = MSet.of_list forbid in
  { stdlib; recurse; exclude; ignore; forbid }

let to_ignore ~cfg modname = MSet.mem modname cfg.ignore

type providers = ?crc:Digest.t -> Modname.t -> Info.t option
type disambiguate = Modname.t -> Info.t list -> Info.t

let absolute =
  (* NOTE(dinosaure): [Fpath.v] should be fine! *)
  let cwd = Fpath.v (Sys.getcwd ()) in
  fun path ->
    let path = if Fpath.is_rel path then Fpath.(cwd // path) else path in
    Fpath.normalize path

exception Multiple_solutions of Modname.t * Digest.t option * Info.t list

let () =
  let dummy = String.make (Digest.length * 2) '-' in
  Printexc.register_printer @@ function
  | Multiple_solutions (modname, crc, infos) ->
      somef "Multiple solutions for %a (%a): @[<hov>%a@]" Modname.pp modname
        Fmt.(option ~none:(const string dummy) Digest.pp)
        crc
        Fmt.(list ~sep:(any ";@ ") Info.pp)
        infos
  | _ -> None

(* NOTE(dinosaure): The purpose of this function is to reclassify
   dependencies based on what has just been injected. For example, adding
   [cmdliner.cmi] means that the [Cmd] module and the [Term] module can be
   resolved without requiring any further information. *)
let prune ?disambiguate infos modules =
  let fn0 (m, crc) (p, crc') =
    match (crc, crc') with
    | Some crc, Some crc' when Digest.equal crc crc' ->
        let part = Info.Path.singleton m in
        Info.Path.is_a_part ~part p
    | Some _, Some _ -> false
    | Some _, None -> false
    | None, Some _ | None, None ->
        let part = Info.Path.singleton m in
        Info.Path.is_a_part ~part p
  in
  let fn1 (m, crc) info =
    let exports = Info.exports info in
    let found = List.exists (fn0 (m, crc)) exports in
    if found then Some info else None
  in
  let take (infos, _, rem) crc m solution =
    Log.debug (fun mf -> mf "take %a for %a" Info.pp solution Modname.pp m);
    let location = Info.location solution in
    let fn info = Info.qualify info ~location ?crc `Intf m in
    (List.map fn infos, true, rem)
  in
  let fn0 (infos, progress, rem) (m, crc) =
    match List.filter_map (fn1 (m, crc)) infos with
    | [ solution ] -> take (infos, progress, rem) crc m solution
    | _ :: _ as solutions -> (
        (* The same module is exported by several collected interfaces (e.g.
           [Term] by both [cmdliner] and [mnotty]): defer to [disambiguate]
           when the caller provides one, otherwise report the ambiguity. *)
        match disambiguate with
        | Some choose -> take (infos, progress, rem) crc m (choose m solutions)
        | None -> raise (Multiple_solutions (m, crc, solutions)))
    | [] -> (infos, progress, (m, crc) :: rem)
  in
  let rec go infos modules =
    let infos, progress, modules =
      List.fold_left fn0 (infos, false, []) modules
    in
    if progress then go infos modules else (infos, modules)
  in
  go infos modules

let missing_intfs infos =
  List.concat_map (fun info -> fst (Info.missing info)) infos

let not_in_forbidden_modules cfg modules =
  match List.filter (fun (m, _) -> MSet.mem m cfg.forbid) modules with
  | [] -> Ok ()
  | forbidden ->
      error_msgf "@[<hov>the project requires forbidden module(s): %a@]"
        Fmt.(list ~sep:(any ",@ ") Modname.pp)
        (List.map fst forbidden)

let sort = List.sort_uniq (fun (a, _) (b, _) -> Modname.compare a b)

let solve_intfs ?disambiguate ~cfg:({ recurse; exclude; stdlib; _ } as cfg)
    ~providers dirs =
  let ( let* ) = Result.bind in
  let fn = Uniq_resolve.Src.sources ~recurse ~exclude in
  let dirs = List.map absolute dirs in
  let dirs = List.map Fpath.to_dir_path dirs in
  let srcs = List.map fn dirs in
  let fn (infos, progress, rem) (m, crc) =
    begin match providers ?crc m with
    | None -> (infos, progress, (m, crc) :: rem)
    | Some solution when List.exists (Info.equal solution) infos = false ->
        let location = Info.location solution in
        (* NOTE(dinosaure): here, [crc] is still the one from what it miss
           but, with the solution, we can actually fix it with what [solution]
           gives to us. We can observe some requalification from [Location] to
           [Fully_qualified] which is fine but I suspect an override of the
           [crc] sometimes... *)
        let fn info = Info.qualify info ~location ?crc `Intf m in
        let infos = List.map fn infos in
        Log.debug (fun m -> m "add %a" Info.pp solution);
        (solution :: infos, true, rem)
    | Some _solution ->
        (* NOTE(dinosaure): The provider points at an artifact we already hold,
           yet the module is still unresolved (typically a digest mismatch).
           Leave it pending so it surfaces as a hole rather than aborting. *)
        (infos, progress, (m, crc) :: rem)
    end
  in
  let rec go infos =
    match missing_intfs infos |> sort with
    | [] -> Ok (infos, [])
    | modules ->
        let* () = not_in_forbidden_modules cfg modules in
        let infos, modules = prune ?disambiguate infos modules in
        let modules = sort modules in
        let infos, progress, modules =
          match modules with
          | [] -> (infos, true, [])
          | modules -> List.fold_left fn (infos, false, []) modules
        in
        if progress then go infos else Ok (infos, modules)
  in
  let* infos = Uniq_resolve.qualify ~stdlib srcs in
  let* infos, modules =
    try go infos
    with Multiple_solutions (m, _crc, solutions) ->
      error_msgf "@[<v>%a is provided by several incompatible interfaces:@,%a@]"
        Modname.pp m
        Fmt.(list ~sep:cut (any "  " ++ Info.pp))
        solutions
  in
  (* NOTE(dinosaure): delete modules that we can ignore. *)
  let fn (m, _) = not (MSet.mem m cfg.ignore) in
  let modules = List.filter fn modules in
  Ok (infos, modules)
