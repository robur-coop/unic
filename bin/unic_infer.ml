module Meta = Uniq_meta
module Info = Uniq_info
module Solver = Uniq_solver

let prompt modname pkgs =
  let pp_pkg_with_idx ppf (idx, pkg) =
    Fmt.pf ppf "  [%d] %a" idx Uniq_meta.Path.pp pkg
  in
  let pkgs_with_idx = List.mapi (fun idx pkg -> (idx, pkg)) pkgs in
  Fmt.pr "@[<v>Module %a is provided by several ocamlfind packages:@,%a@]@."
    Modname.pp modname
    Fmt.(list ~sep:cut pp_pkg_with_idx)
    pkgs_with_idx;
  Fmt.pr "Pick one [0-%d]: %!" (List.length pkgs - 1);
  match input_line stdin with
  | exception End_of_file -> raise (Uniq_solver.Ambiguous (modname, pkgs))
  | line ->
      begin match int_of_string_opt line with
      | Some idx when idx >= 0 && idx < List.length pkgs -> List.nth pkgs idx
      | _ -> raise (Uniq_solver.Ambiguous (modname, pkgs))
      end

let has_c_stubs node = List.exists Uniq_info.has_c_stubs node.Solver.objs

let pp_elt ppf (pkg, dirpath) =
  Fmt.pf ppf "%a(%a)" Meta.Path.pp pkg Fpath.pp dirpath

let pp_node ppf (pkg, { Solver.dirpath; _ }) =
  Fmt.pf ppf "%a(%a)" Meta.Path.pp pkg Fpath.pp dirpath

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

let their_are_copies = function
  | [] -> true
  | witness :: rem ->
      let e = witness.Info.exports in
      let rem = List.map (fun info -> info.Info.exports) rem in
      let fn0 (m, crc) (m', crc') =
        match (crc, crc') with
        | Some crc, Some crc' ->
            Uniq_digest.equal crc crc' && Modname.compare m m' = 0
        | _, _ -> false
      in
      let fn1 e' = try List.for_all2 fn0 e e' with _ -> false in
      List.for_all fn1 rem

(* NOTE(dinosaure): [List.length solutions >= 1] *)
let prefer_stdlib ?stdlib solutions =
  match stdlib with
  | None -> List.hd solutions
  | Some dir ->
      let dir = Fpath.(normalize (to_dir_path dir)) in
      let in_stdlib info =
        let where = Uniq_info.location info in
        Fpath.equal dir Fpath.(normalize (to_dir_path (parent where)))
      in
      List.find_opt in_stdlib solutions
      |> Stdlib.Option.value ~default:(List.hd solutions)

let pp_module ppf (modname, crc) =
  match crc with
  | Some crc -> Fmt.pf ppf "%a(%a)" Modname.pp modname Uniq_digest.pp crc
  | None -> Modname.pp ppf modname

module Archive = struct
  type t = Meta.archive

  let compare = Stdlib.compare
end

module ASet = Set.Make (Archive)

let run _quiet cfg0 cfg1 dirs =
  let ( let* ) = Result.bind in
  let stdlib =
    match cfg0 with
    | Some cfg -> Uniq_cfg.get cfg ~key:"standard_library" Uniq_cfg.Value.path
    | None -> None
  in
  let native =
    match cfg0 with
    | Some (where, _) ->
        let base = Fpath.basename (where :> Fpath.t) in
        not (String.length base >= 6 && String.sub base 0 6 = "ocamlc")
    | None -> true
  in
  let predicates = if native then [ "native" ] else [ "byte" ] in
  let cfg =
    Uniq_solver.Ng.config ~stdlib:cfg1.Solver.Config.stdlib
      ~recurse:cfg1.Solver.Config.recurse ~exclude:cfg1.Solver.Config.exclude
      ~forbid:(Solver.MSet.elements cfg1.Solver.Config.forbid)
      ()
  in
  let roots = cfg1.Solver.Config.roots in
  let* gamma = search_cmis ~roots in
  let providers ?crc modname =
    let fn _filepath info =
      let exports = info.Uniq_info.exports in
      let fn (modname', crc') =
        match (crc, crc') with
        | Some crc, Some crc' when Uniq_digest.equal crc crc' ->
            Modname.compare modname modname' = 0
        | Some _, Some _ -> false
        | None, Some _ | Some _, None | None, None ->
            Modname.compare modname modname' = 0
      in
      if List.exists fn exports then Some info else None
    in
    let solutions = Fpath.Map.filter_map fn gamma in
    let solutions = Fpath.Map.bindings solutions in
    let solutions = List.map snd solutions in
    match solutions with
    | [ info ] -> Some info
    | [] -> None
    | _ :: _ as solutions when their_are_copies solutions ->
        (* NOTE(dinosaure): we can have similar artifacts from [ocaml] and
           [ocaml-solo5] when its about the standard library. To not mislead
           next steps, we take the right artifact according to our toolchain
           configuration even though we can choose any of the options without a
           doubt! *)
        Some (prefer_stdlib ?stdlib solutions)
    | _ :: _ as solutions ->
        Logs.err (fun m -> m "Multiple solutions for %a" Modname.pp modname);
        Logs.err (fun m ->
            m "@[<hov>%a@]" Fmt.(list ~sep:(any ",@ ") Info.pp) solutions);
        assert false
  in
  let* infos, private_modules = Solver.Ng.solve_intfs ~cfg ~providers dirs in
  let intfs = List.filter Uniq_info.is_a_cmi infos in
  let pkgs = Meta.packages_with_archive roots in
  let* descrs =
    let fn acc info =
      let* acc = acc in
      let* pkg =
        Meta.from_cmi_to_impl ~roots ~packages:pkgs ?stdlib
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
  let* impls =
    let fn acc archive =
      let* acc = acc in
      let* infos = Meta.archives_of ~roots ~predicates archive in
      Ok (List.rev_append infos acc)
    in
    List.fold_left fn (Ok []) archives
  in
  let provided (modname, crc) =
    let in_info info =
      let exists =
        let fn (path, _) =
          match Uniq_info.Path.to_list path with
          | [ m ] -> Modname.compare m modname = 0
          | _ -> false
        in
        List.exists fn (Uniq_info.exports info)
      in
      let matches =
        match crc with
        | None -> true
        | Some crc ->
            let check = Uniq_digest.equal crc in
            let fn (m, c) =
              Modname.compare m modname = 0
              && Stdlib.Option.(map check c |> value ~default:true)
            in
            List.exists fn (Uniq_info.intfs_imported info)
      in
      exists && matches
    in
    List.exists in_info impls
  in
  let unresolved = List.filter (Fun.negate provided) private_modules in
  Fmt.pr
    ">>> Static-link graph (%s), %d interface(s) + %d implementation(s):\n%!"
    (if native then "native" else "bytecode")
    (List.length intfs) (List.length impls);
  List.iter (fun info -> Fmt.pr "  %a\n%!" Info.pp info) impls;
  if unresolved <> [] then begin
    Fmt.pr ">>> Unresolved private modules: @[<hov>%a@]\n%!"
      Fmt.(list ~sep:(any ",@ ") pp_module)
      unresolved
  end;
  Ok ()

let run _quiet _cfg0 cfg1 dirs =
  match run _quiet _cfg0 cfg1 dirs with
  | Ok () -> 0
  | Error (`Msg msg) ->
      Fmt.epr "%s: %s\n%!" Sys.executable_name msg;
      exit 1

open Cmdliner
open Unic_cli

let without_stdlib =
  let doc = "Do not add the standard library to the list of include sources." in
  Arg.(value & flag & info [ "without-stdlib" ] ~doc)

let recurse =
  let doc = "Include sub-directories." in
  Arg.(value & flag & info [ "r"; "recurse" ] ~doc)

let exclude =
  let doc =
    "Exlude a file, or a directory (and its sub-directories), from resolution."
  in
  let v = path in
  Arg.(value & opt_all v [] & info [ "x"; "exclude" ] ~doc ~docv:"PATH")

let ignore =
  let doc =
    "Do not require a provider for this module (e.g. a generated unit). \
     Without it, a module no package provides is an error. Repeatable or \
     comma-separated."
  in
  let open Arg in
  value & opt_all (list modname) [] & info [ "i"; "ignore" ] ~doc ~docv:"MODULE"

let forbid =
  let doc =
    "Forbid this module: referencing it is an error even if a package provides \
     it. Repeatable or comma-separated."
  in
  let open Arg in
  value & opt_all (list modname) [] & info [ "forbid" ] ~doc ~docv:"MODULE"

let dirs =
  let doc = "The OCaml project directories." in
  Arg.(non_empty & pos_all existing_dirpath [] & info [] ~doc ~docv:"DIRECTORY")

let setup_solver without_stdlib recurse exclude ignore forbid policy roots =
  let ignore = List.concat ignore in
  let forbid = List.concat forbid in
  Uniq_solver.Config.cfg ~stdlib:(not without_stdlib) ~recurse ~exclude ~ignore
    ~forbid ~policy roots

let setup_solver =
  let open Term in
  const setup_solver
  $ without_stdlib
  $ recurse
  $ exclude
  $ ignore
  $ forbid
  $ setup_policy
  $ setup_ocamlfind

let term =
  let open Term in
  const run $ setup_logs $ setup_ocaml $ setup_solver $ dirs

let cmd =
  let doc = "Infer the opam package an OCaml project should vendor." in
  let man = [ `S Manpage.s_description ] in
  let info = Cmd.info "infer" ~doc ~man in
  Cmd.v info term
