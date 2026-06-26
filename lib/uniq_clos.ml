module Info = Uniq_info
module Meta = Uniq_meta
module MSet = Set.Make (Modname)

module Archive = struct
  type t = Meta.archive

  let compare = Stdlib.compare
end

module ASet = Set.Make (Archive)

type env = {
    stdlib: Fpath.t option
  ; predicates: string list
  ; gamma: Info.t Fpath.Map.t
  ; roots: Fpath.t list
  ; pkgs: Meta.package list
}

type disambiguate = Modname.t -> Meta.Path.t list -> Meta.Path.t
type intf = Modname.t * Info.t
type impl = Modname.t * Info.t

let search_cmis ~roots =
  let elements path =
    if Sys.is_directory (Fpath.to_string path) then Ok false
    else if Fpath.mem_ext [ ".cmi" ] path then Ok true
    else Ok false
  in
  let traverse path =
    let fn root = Fpath.is_rooted ~root path || Fpath.equal root path in
    let traverse = List.exists fn roots in
    Ok traverse
  in
  let fn path acc =
    match Uniq_info.v path with
    | Ok info when Uniq_info.is_a_cmi info -> Fpath.Map.add path info acc
    | Ok _ | Error _ -> acc
  in
  let err _path _ = Ok () in
  Bos.OS.Path.fold ~err ~dotfiles:false ~elements:(`Sat elements)
    ~traverse:(`Sat traverse) fn Fpath.Map.empty roots

let env ?cfg roots =
  let ( let* ) = Result.bind in
  let stdlib =
    match cfg with
    | Some cfg -> Uniq_cfg.get cfg ~key:"standard_library" Uniq_cfg.Value.path
    | None -> None
  in
  let native =
    match cfg with
    | Some (where, _) ->
        let base = Fpath.basename (where :> Fpath.t) in
        not (String.length base >= 6 && String.sub base 0 6 = "ocamlc")
    | None -> true
  in
  let predicates = if native then [ "native" ] else [ "byte" ] in
  let* gamma = search_cmis ~roots in
  let pkgs = Meta.packages_with_archive roots in
  Ok { stdlib; predicates; gamma; roots; pkgs }

let gamma { gamma; _ } = gamma
let stdlib { stdlib; _ } = stdlib

let close_impls ~resolve intfs impls =
  let ( let* ) = Result.bind in
  let key info = Fpath.to_string (Info.location info) in
  let provided infos =
    let add_unit set (path, _) =
      match Info.Path.to_list path with [ m ] -> MSet.add m set | _ -> set
    in
    let add_info set info = List.fold_left add_unit set (Info.exports info) in
    List.fold_left add_info MSet.empty infos
  in
  let imported impls =
    let tbl = Hashtbl.create 0x7ff in
    let fn info =
      let impl (m, _) =
        if not (Hashtbl.mem tbl m) then Hashtbl.add tbl m (m, None)
      in
      let intf (m, crc) = Hashtbl.replace tbl m (m, crc) in
      List.iter impl (Info.impls_imported info);
      List.iter intf (Info.intfs_imported info)
    in
    List.iter fn impls;
    Hashtbl.fold (fun _ mc acc -> mc :: acc) tbl []
  in
  let seen = Hashtbl.create 0x7ff in
  List.iter (fun info -> Hashtbl.replace seen (key info) ()) impls;
  let rec go attempted impls =
    let known = provided (List.rev_append intfs impls) in
    let missing =
      imported impls
      |> List.filter (fun (m, _) ->
          (not (MSet.mem m known)) && not (MSet.mem m attempted))
    in
    match missing with
    | [] -> Ok impls
    | missing ->
        let fn acc (m, crc) =
          let* attempted, added = acc in
          let attempted = MSet.add m attempted in
          let* archives = resolve (m, crc) in
          let archives =
            List.filter (fun info -> not (Hashtbl.mem seen (key info))) archives
          in
          List.iter (fun info -> Hashtbl.replace seen (key info) ()) archives;
          Ok (attempted, List.rev_append archives added)
        in
        let* attempted, added =
          List.fold_left fn (Ok (attempted, [])) missing
        in
        if added = [] then Ok impls
        else go attempted (List.rev_append added impls)
  in
  go MSet.empty impls

let their_are_copies = function
  | [] -> true
  | witness :: rem ->
      let e = witness.Info.exports in
      let rem = List.map (fun info -> info.Info.exports) rem in
      let fn0 (m, crc) (m', crc') =
        match (crc, crc') with
        | Some crc, Some crc' ->
            Digest.equal crc crc' && Modname.compare m m' = 0
        | _, _ -> false
      in
      let fn1 e' = try List.for_all2 fn0 e e' with _ -> false in
      List.for_all fn1 rem

let prefer_stdlib ?stdlib solutions =
  match stdlib with
  | None -> List.hd solutions
  | Some dir ->
      let dir = Fpath.(normalize (to_dir_path dir)) in
      let in_stdlib info =
        let where = Info.location info in
        Fpath.equal dir Fpath.(normalize (to_dir_path (parent where)))
      in
      List.find_opt in_stdlib solutions
      |> Stdlib.Option.value ~default:(List.hd solutions)

let resolve ~env:{ stdlib; predicates; gamma; roots; pkgs } ~disambiguate
    (modname, crc) =
  let ( let* ) = Result.bind in
  let matches_crcs crc crc' =
    match (crc, crc') with
    | Some a, Some b -> Digest.equal a b
    | Some _, None | None, Some _ | None, None -> true
  in
  let matches info =
    let fn (m, crc') = Modname.compare modname m = 0 && matches_crcs crc crc' in
    List.exists fn info.Info.exports
  in
  let fn _ info acc = if matches info then info :: acc else acc in
  let candidates = Fpath.Map.fold fn gamma [] in
  let cmi =
    match candidates with
    | [] -> None
    | [ info ] -> Some info
    | _ :: _ as all when their_are_copies all ->
        Some (prefer_stdlib ?stdlib all)
    | _ :: _ ->
        Logs.warn (fun m ->
            m "Ambiguous implementation dependency %a; skipping" Modname.pp
              modname);
        None
  in
  match cmi with
  | None -> Ok []
  | Some cmi -> begin
      let* archive =
        Meta.from_cmi_to_impl ~roots ~packages:pkgs ?stdlib ~disambiguate
          (Info.location cmi)
      in
      match archive with
      | None -> Ok []
      | Some archive -> Meta.archives_of ~roots ~predicates archive
    end

let impls_from_intfs ~env:{ stdlib; roots; pkgs; predicates; _ } ~disambiguate
    intfs =
  let ( let* ) = Result.bind in
  let* descrs =
    let fn acc info =
      let* acc = acc in
      let* pkg =
        Meta.from_cmi_to_impl ~roots ~packages:pkgs ?stdlib ~disambiguate
          (Uniq_info.location info)
      in
      Ok ((info, pkg) :: acc)
    in
    List.fold_left fn (Ok []) intfs
  in
  let archives =
    let fn acc (_, pkg) =
      let none = acc and some a = ASet.add a acc in
      Stdlib.Option.fold ~none ~some pkg
    in
    List.fold_left fn ASet.empty descrs |> ASet.elements
  in
  let fn acc archive =
    let* acc = acc in
    let* infos = Meta.archives_of ~roots ~predicates archive in
    Ok (List.rev_append infos acc)
  in
  List.fold_left fn (Ok []) archives

let impls ~env ~disambiguate infos =
  let ( let* ) = Result.bind in
  let intfs = List.filter Info.is_a_cmi infos in
  let* impls = impls_from_intfs ~env ~disambiguate intfs in
  let resolve = resolve ~env ~disambiguate in
  close_impls ~resolve intfs impls

let verify ~env ~disambiguate infos =
  let ( let* ) = Result.bind in
  let intfs = List.filter Info.is_a_cmi infos in
  let* impls = impls_from_intfs ~env ~disambiguate intfs in
  let resolve = resolve ~env ~disambiguate in
  let* impls = close_impls ~resolve intfs impls in
  let sources = List.filter (Fun.negate Info.is_a_cmi) infos in
  let names infos =
    let add_unit set (path, _) =
      match List.rev (Info.Path.to_list path) with
      | leaf :: _ -> MSet.add leaf set
      | [] -> set
    in
    let add_info set info = List.fold_left add_unit set (Info.exports info) in
    List.fold_left add_info MSet.empty infos
  in
  let intf_provided = names (List.concat [ sources; intfs; impls ]) in
  let impl_provided = names (List.rev_append sources impls) in
  let holes imported_of provided nodes =
    let tbl = Hashtbl.create 0x7ff in
    let fn info =
      let fn (m, _) =
        if (not (MSet.mem m provided)) && not (Hashtbl.mem tbl m) then
          Hashtbl.add tbl m (m, info)
      in
      List.iter fn (imported_of info)
    in
    List.iter fn nodes;
    Hashtbl.fold (fun _ hole acc -> hole :: acc) tbl []
  in
  let intf_holes =
    holes Info.intfs_imported intf_provided
      (List.concat [ sources; intfs; impls ])
  in
  let impl_holes =
    holes Info.impls_imported impl_provided (List.rev_append sources impls)
  in
  Ok (intf_holes, impl_holes)
