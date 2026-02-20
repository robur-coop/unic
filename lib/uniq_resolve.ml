let src = Logs.Src.create "uniq.qualify"

module Log = (val Logs.src_log src : Logs.LOG)
module Digest = Uniq_digest
module Info = Uniq_info

type _ Effect.t +=
  | Choose : Modname.t * Info.t list -> Info.t Effect.t
  | Conflict : Digest.t * Info.t * Info.t -> Info.t Effect.t

let conflict crc t t' = Effect.perform (Conflict (crc, t, t'))

let by_crc gamma =
  let add u m crc =
    match Digest.Map.find_opt crc m with
    | None -> Digest.Map.add crc u m
    | Some v -> (
        match (Info.is_a_library u, Info.is_a_library v) with
        | true, false -> Digest.Map.add crc u m
        | false, true -> Digest.Map.add crc v m
        | _ ->
            if Info.is_a_cmi u && Info.is_a_cmi v then Digest.Map.add crc u m
            else begin
              Logs.err (fun m ->
                  m "Conflict on %a between %a and %a" Digest.pp crc Info.pp u
                    Info.pp v);
              let t = conflict crc u v in
              Digest.Map.add crc t m
            end)
  in
  let fold m t =
    List.filter_map snd (Info.exports t) |> List.fold_left (add t) m
  in
  List.fold_left fold Digest.Map.empty gamma

let choose modname vs =
  Log.debug (fun m ->
      m "Choose %a from: @[<hov>%a@]" Modname.pp modname
        Fmt.(Dump.list Info.pp)
        vs);
  match List.filter Info.is_a_library vs with
  | [] -> Effect.perform (Choose (modname, vs))
  | [ v ] -> v
  | ws -> (
      match List.filter Info.is_native ws with
      | [] -> Effect.perform (Choose (modname, vs))
      | [ w ] -> w
      | _ -> Effect.perform (Choose (modname, vs)))

let flip f y x = f x y
let ( $ ) f x = f x

let exports m v =
  let fn (m', _) =
    match Uniq_info.Path.to_list m' with
    | [ m' ] -> Modname.compare m m' = 0
    | _ -> false
  in
  List.exists fn (Info.exports v)

let qualify_by_crc gamma =
  let by_crc = by_crc gamma in
  let qualify k (by_modname, t) (modname, crc) =
    Log.debug (fun m ->
        m "Search %a for %a" Fmt.(Dump.option Digest.pp) crc Info.pp t);
    match Stdlib.Option.bind crc (flip $ Digest.Map.find_opt $ by_crc) with
    | None -> (by_modname, t)
    | Some v when exports modname v ->
        let by_modname =
          match k with
          | `Impl ->
              (fst by_modname, Modname.Map.add modname [ v ] (snd by_modname))
          | `Intf ->
              (Modname.Map.add modname [ v ] (fst by_modname), snd by_modname)
        in
        let location = Info.location v in
        let t = Info.qualify t ~location ?crc k modname in
        (by_modname, t)
    | Some _v ->
        Logs.err (fun m -> m "%a => %a" Modname.pp modname Info.pp _v);
        assert false
  in
  let fold (by_modname, ts) t =
    (by_modname, t)
    |> (flip $ List.fold_left (qualify `Intf) $ Info.intfs_imported t)
    |> (flip $ List.fold_left (qualify `Impl) $ Info.impls_imported t)
    |> fun (by_modname, t) -> (by_modname, t :: ts)
  in
  List.fold_left fold (Modname.Map.(empty, empty), []) gamma

let complete_by_modname by_modname gamma =
  let add t m (modname, _crc) =
    match Modname.Map.find_opt modname m with
    | Some [ t' ] when Stdlib.Option.is_some (Info.crc_of t' modname) -> m
    | None -> Modname.Map.add modname [ t ] m
    | Some ts -> Modname.Map.add modname (t :: ts) m
  in
  let fold (intfs, impls) t =
    match Info.kind t with
    | `Impl -> (intfs, List.fold_left (add t) impls t.Info.exports)
    | `Intf -> (List.fold_left (add t) intfs t.Info.exports, impls)
  in
  List.fold_left fold by_modname gamma |> fun (intfs, impls) ->
  let only _ = function [ t ] -> Some t | _ -> None in
  let restricted_intfs = Modname.Map.filter_map only intfs in
  let restricted_impls = Modname.Map.filter_map only impls in
  ((restricted_intfs, restricted_impls), (intfs, impls))

let qualify_objects gamma =
  let by_modname, gamma = qualify_by_crc gamma in
  let restricted, extended = complete_by_modname by_modname gamma in
  let qualify k (gamma, t) (modname, _crc) =
    Log.debug (fun m -> m "Search %a for %a" Modname.pp modname Info.pp t);
    let restricted, extended =
      match k with
      | `Intf -> (fst gamma, fst extended)
      | `Impl -> (snd gamma, snd extended)
    in
    match
      ( Modname.Map.find_opt modname restricted
      , Modname.Map.find_opt modname extended )
    with
    | None, None -> (gamma, t)
    | Some v, _ ->
        let location = Info.location v in
        let crc = Info.crc_of v modname in
        (gamma, Info.qualify t ~location ?crc k modname)
    | None, Some vs ->
        let v = choose modname vs in
        let location = Info.location v in
        let crc = Info.crc_of v modname in
        let gamma =
          match k with
          | `Intf -> (Modname.Map.add modname v (fst gamma), snd gamma)
          | `Impl -> (fst gamma, Modname.Map.add modname v (snd gamma))
        in
        (gamma, Info.qualify t ~location ?crc k modname)
  in
  let is_none = Stdlib.Option.is_none in
  let fold (gamma, ts) t =
    let unqualified_intfs =
      List.filter (Fun.compose is_none snd) (Info.intfs_imported t)
    in
    let unqualified_impls =
      List.filter (Fun.compose is_none snd) (Info.impls_imported t)
    in
    (gamma, t)
    |> (flip $ List.fold_left (qualify `Intf) $ unqualified_intfs)
    |> (flip $ List.fold_left (qualify `Impl) $ unqualified_impls)
    |> fun (by_modname, t) -> (by_modname, t :: ts)
  in
  List.fold_left fold (restricted, []) gamma |> snd

module Src = struct
  type directory = { recurse: bool; location: Fpath.t }
  type t = [ `File of Fpath.t | `Sources of directory | `Objects of directory ]

  let pp ppf = function
    | `File v -> Fpath.pp ppf v
    | `Sources { recurse= true; location } ->
        Fmt.pf ppf "Y(%a:*.{ml,mli,cmi,cmo,cmx,cma,cmxa})" Fpath.pp location
    | `Sources { recurse= false; location } ->
        Fmt.pf ppf "(%a:*.{cmi,cmo,cmx,cma,cmxa})" Fpath.pp location
    | `Objects { recurse= true; location } ->
        Fmt.pf ppf "Y(%a:*.{cmi,cmo,cmx,cma,cmxa})" Fpath.pp location
    | `Objects { recurse= false; location } ->
        Fmt.pf ppf "(%a:*.{cmi,cmo,cmx,cma,cmxa})" Fpath.pp location

  let file location =
    if
      Fpath.is_file_path location = false
      || Sys.file_exists (Fpath.to_string location) = false
      || Sys.is_directory (Fpath.to_string location) = true
    then Fmt.invalid_arg "%a is not a file" Fpath.pp location
    else
      let sources = [ ".ml"; ".mli" ] in
      let objects = [ ".cmi" ] in
      if Fpath.mem_ext sources location then `File location
      else if Fpath.mem_ext objects location then
        let is_cmi ic () =
          match Misc.Magic_number.read_info ic with
          | Ok { Misc.Magic_number.kind= Cmi; _ } -> true
          | _ | (exception _) -> false
        in
        match Bos.OS.File.with_ic location is_cmi () with
        | Ok true -> `File location
        | Ok false | Error _ ->
            Fmt.invalid_arg "Invalid .cmi file: %a" Fpath.pp location
      else Fmt.invalid_arg "Invalid extension of %a" Fpath.pp location

  let sources ?(recurse = true) location =
    if
      Fpath.is_dir_path location = false
      || Sys.file_exists (Fpath.to_string location) = false
      || Sys.is_directory (Fpath.to_string location) = false
    then Fmt.invalid_arg "%a is not a directory" Fpath.pp location
    else `Sources { recurse; location }

  let objects ?(recurse = false) location =
    if
      Fpath.is_dir_path location = false
      || Sys.file_exists (Fpath.to_string location) = false
      || Sys.is_directory (Fpath.to_string location) = false
    then Fmt.invalid_arg "%a is not a directory" Fpath.pp location
    else `Objects { recurse; location }
end

open Bos

let sources_to_locations (`Sources { Src.recurse; location }) =
  let traverse =
    if recurse then `Any
    else
      let fn location' = Ok (Fpath.equal location location') in
      `Sat fn
  in
  let elements =
    let sources = [ ".ml"; ".mli"; ".cmo"; ".cmi"; ".cma"; ".cmx"; "cmxa" ] in
    let fn location = Ok (Fpath.mem_ext sources location) in
    `Sat fn
  in
  OS.Path.fold ~dotfiles:true ~elements ~traverse List.cons [] [ location ]

let objects_to_locations (`Objects { Src.recurse; location }) =
  let traverse =
    if recurse then `Any
    else
      let fn location' = Ok (Fpath.equal location location') in
      `Sat fn
  in
  let elements =
    let sources = [ ".cmo"; ".cma"; ".cmx"; ".cmxa"; ".cmi" ] in
    let fn location = Ok (Fpath.mem_ext sources location) in
    `Sat fn
  in
  OS.Path.fold ~dotfiles:true ~elements ~traverse List.cons [] [ location ]

let to_locations = function
  | `File location -> Ok [ location ]
  | `Sources location -> sources_to_locations (`Sources location)
  | `Objects location -> objects_to_locations (`Objects location)

exception No_common_prefix of Fpath.t list

let no_common_prefix paths = raise (No_common_prefix paths)

let common_path paths =
  let fold prefix p =
    match Fpath.find_prefix prefix p with
    | Some prefix' ->
        if Fpath.compare prefix' prefix < 0 then prefix' else prefix
    | None -> no_common_prefix paths
  in
  match paths with
  | [ path ] -> fst (Fpath.split_base path)
  | prefix :: paths -> List.fold_left fold prefix paths
  | [] -> assert false (* XXX(dinosaure): see [from_sources]. *)

let from_sources ~cmis = function
  | [] -> []
  | sources ->
      let current = common_path sources in
      let sources = List.rev_append sources (List.map Info.location cmis) in
      let sources =
        List.map
          begin fun p ->
            Stdlib.Option.value ~default:p Fpath.(rem_prefix current p)
          end
          sources
      in
      let sources =
        List.map (Fun.compose Unitname.modulize Fpath.to_string) sources
      in
      let sources = List.map Unitname.filepath sources in
      let { Unit.ml; Unit.mli } = Uniq_ml.run_into ~current sources in
      List.map
        begin fun u ->
          let intfs, impls =
            Deps.all u.Unit.more.Unit.dependencies
            |> List.fold_left Info.to_elt ([], [])
          in
          let location = Fpath.v (Namespaced.filepath u.Unit.src.Pkg.file) in
          let name = Unitname.modulize (Fpath.to_string location) in
          assert (Unitname.modname name = Namespaced.module_name u.Unit.path);
          let kind =
            match Support.extension (Fpath.to_string location) with
            | "ml" -> M2l.Structure
            | "mli" | "cmi" -> M2l.Signature
            | _ -> assert false
          in
          let format =
            match kind with
            | M2l.Structure -> Info.Format (Ml, u)
            | M2l.Signature -> Info.Format (Mli, u)
          in
          let version = None in
          let exports = [ (Unitname.modname name, None) ] in
          let modules = Uniq_info.Path.Set.empty in
          (* TODO *)
          { Info.name; version; modules; exports; intfs; impls; format }
        end
        (List.rev_append ml mli)
      |> List.filter (Fun.negate Info.is_a_cmi)

let ( let* ) = Result.bind
let failwith_error_msg = function Ok v -> v | Error (`Msg msg) -> failwith msg

let qualify files =
  let part location =
    OS.File.with_ic location @@ fun ic () ->
    match Misc.Magic_number.read_info ic with
    | Error _ -> Ok (Either.Right location)
    | Ok info ->
        let* v = Info.from_object location info ic in
        Ok (Either.Left v)
  in
  let part loc = Result.join (part loc ()) |> failwith_error_msg in
  let objects, sources = List.partition_map part files in
  let objects = qualify_objects objects in
  let cmis, _ = List.partition Info.is_a_cmi objects in
  let sources = from_sources ~cmis sources in
  List.rev_append objects sources

let qualify lst =
  let fn acc x =
    let* loc = to_locations x in
    let* acc = acc in
    Ok (loc :: acc)
  in
  let* locations = List.fold_left fn (Ok []) lst in
  let locations = List.concat locations in
  try Ok (qualify locations) with Failure msg -> Error (`Msg msg)
