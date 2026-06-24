let src = Logs.Src.create "uniq.vendor"

module Log = (val Logs.src_log src : Logs.LOG)
module Meta = Uniq_meta
module Solver = Uniq_solver

type node = {
    has_c_stubs: bool ref
  ; deps: (Meta.Path.t * bool ref) list
  ; dirpath: Fpath.t
}

let has_c_stubs node = List.exists Uniq_info.has_c_stubs node.Solver.objs

let color (g : Solver.graph) =
  let pkgs = Hashtbl.create 0x7ff in
  let rec create pkg (node : Solver.node) =
    match Hashtbl.find_opt pkgs pkg with
    | Some node -> (pkg, node)
    | None ->
        let has_c_stubs = ref (has_c_stubs node) in
        let fn (pkg, _) =
          match Meta.Path.Map.find_opt pkg g with
          | Some node ->
              let _, { has_c_stubs; _ } = create pkg node in
              Some (pkg, has_c_stubs)
          | None -> None
        in
        let deps = List.filter_map fn node.Solver.deps in
        let node = { has_c_stubs; deps; dirpath= node.Solver.dirpath } in
        Hashtbl.replace pkgs pkg node;
        (pkg, node)
  in
  let fn pkg node acc = create pkg node :: acc in
  let pkgs = Meta.Path.Map.fold fn g [] in
  let fn (pkg, { has_c_stubs; deps; dirpath }) =
    let fn (_, has_c_stubs) = !has_c_stubs in
    let trans = List.exists fn deps in
    Log.debug (fun m ->
        m "%a: has_c_stubs:%b, trans:%b" Meta.Path.pp pkg !has_c_stubs trans);
    let pp_elt ppf (pkg, ref) = Fmt.pf ppf "%a:%b" Meta.Path.pp pkg !ref in
    Log.debug (fun m ->
        m "%a: @[<hov>%a@]" Meta.Path.pp pkg
          Fmt.(list ~sep:(any ";@ ") pp_elt)
          deps);
    if !has_c_stubs || trans then Some (pkg, dirpath) else None
  in
  List.filter_map fn pkgs
