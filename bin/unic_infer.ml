module Meta = Uniq_meta
module Info = Uniq_info
module Solver = Uniq_solver
module Clos = Uniq_clos
module Vendor = Uniq_vendor
module Opam = Uniq_opam
module Option = Stdlib.Option

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

exception Ambiguous_interface of Modname.t * Info.t list
exception No_input of Modname.t

let rec prompt modname pkgs =
  let pkgs = List.sort Meta.Path.compare pkgs in
  let pp_pkg_with_idx ppf (idx, pkg) =
    Fmt.pf ppf "  [%d] %a" idx Meta.Path.pp pkg
  in
  let pkgs_with_idx = List.mapi (fun idx pkg -> (idx, pkg)) pkgs in
  Fmt.pr "@[<v>Module %a is provided by several ocamlfind packages:@,%a@]@."
    Modname.pp modname
    Fmt.(list ~sep:cut pp_pkg_with_idx)
    pkgs_with_idx;
  Fmt.pr "Pick one [0-%d]: %!" (List.length pkgs - 1);
  match int_of_string_opt (input_line stdin) with
  | exception End_of_file -> raise (No_input modname)
  | Some idx when idx >= 0 && idx < List.length pkgs -> List.nth pkgs idx
  | Some _ | None -> prompt modname pkgs

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
      |> Option.value ~default:(List.hd solutions)

let run _quiet cfg0 roots cfg1 policy dirs =
  let ( let* ) = Result.bind in
  let* env = Uniq_clos.env ?cfg:cfg0 roots in
  let stdlib = Uniq_clos.stdlib env in
  let packages = Meta.packages_with_archive roots in
  let disambiguate_on_packages =
    let memo = Hashtbl.create 0x7ff in
    fun modname paths ->
      match Uniq_policy.disambiguate_with policy modname paths with
      | Some pkg -> pkg
      | None -> (
          match Hashtbl.find_opt memo modname with
          | Some pkg -> pkg
          | None ->
              let pkg = prompt modname paths in
              Hashtbl.replace memo modname pkg;
              pkg)
  in
  let package_of info =
    match
      Meta.from_cmi_to_impl ~roots ~packages ?stdlib
        ~disambiguate:disambiguate_on_packages (Uniq_info.location info)
    with
    | Ok (Some (Meta.Library (path, _, _))) -> Some path
    | Ok (Some (Meta.Stdlib _)) | Ok None | Error _ -> None
  in
  let disambiguate_on_modules modname solutions =
    let with_packages =
      let fn info = Option.map (fun pkg -> (pkg, info)) (package_of info) in
      List.filter_map fn solutions
    in
    match with_packages with
    | [] -> raise (Ambiguous_interface (modname, solutions))
    | _ -> begin
        let pkgs = List.map fst with_packages in
        let chosen = disambiguate_on_packages modname pkgs in
        let fn (pkg, _) = Meta.Path.equal pkg chosen in
        match List.find_opt fn with_packages with
        | Some (_, info) -> info
        | None -> raise (Ambiguous_interface (modname, solutions))
      end
  in
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
    let solutions = Fpath.Map.filter_map fn (Uniq_clos.gamma env) in
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
    | _ :: _ as solutions -> Some (disambiguate_on_modules modname solutions)
  in
  let* infos, _private_modules =
    Solver.solve_intfs ~cfg:cfg1 ~providers
      ~disambiguate:disambiguate_on_modules dirs
  in
  let* intf_holes, impl_holes =
    Clos.verify ~env ~disambiguate:disambiguate_on_packages infos
  in
  let fn (m, _) = Solver.to_ignore ~cfg:cfg1 m in
  let intf_holes = List.filter (Fun.negate fn) intf_holes
  and impl_holes = List.filter (Fun.negate fn) impl_holes in
  match (intf_holes, impl_holes) with
  | [], [] ->
      let* impls =
        Clos.impls ~env ~disambiguate:disambiguate_on_packages infos
      in
      let infos = Vendor.color impls in
      (* NOTE(dinosaure): final pass. The artifacts above are [*.cmxa]
         archives; map them back to the OPAM packages that own them (the ones
         with C stubs and the ones that transitively require them) so the user
         gets package names rather than archive paths.

         We also collect the [-L<dir>] C-link search paths of these archives:
         some dependencies (e.g. [gmp] behind [zarith]) are only reachable
         through the C linker, never through an OCaml module import, so the
         module closure alone misses them. These directories are fragile and
         may be system paths; passing them through [opam_packages_of_meta_dirs]
         is what keeps only the ones actually owned by an installed package. *)
      let dirs =
        let fn info = Fpath.parent (Info.location info) in
        let cmxa_dirs = List.map fn infos in
        let cc_dirs = List.concat_map Info.c_library_dirs infos in
        List.rev_append cc_dirs cmxa_dirs |> List.sort_uniq Fpath.compare
      in
      let* pkgs =
        Opam.with_switch_state @@ fun sw ->
        Opam.package_names_of_meta_dirs ~sw dirs
      in
      Fmt.pr "@[<v>%a@]\n%!" Fmt.(list ~sep:cut string) pkgs;
      Ok ()
  | _ ->
      let pp_hole ppf (m, info) =
        Fmt.pf ppf "  %a (needed by %a)" Modname.pp m Info.pp info
      in
      let pp_section name ppf = function
        | [] -> ()
        | holes -> Fmt.pf ppf "%s:@,%a@," name Fmt.(list ~sep:cut pp_hole) holes
      in
      error_msgf "@[<v>the dependency graph could not be closed:@,%a%a@]"
        (pp_section "missing interfaces")
        intf_holes
        (pp_section "missing implementations")
        impl_holes

let run quiet cfg0 roots cfg1 policy dirs =
  let result =
    try run quiet cfg0 roots cfg1 policy dirs with
    | Ambiguous_interface (modname, solutions) ->
        error_msgf
          "@[<v>%a is provided by several incompatible interfaces:@,%a@]"
          Modname.pp modname
          Fmt.(list ~sep:cut (any "  " ++ Info.pp))
          solutions
    | No_input modname ->
        error_msgf "no answer was given to choose which package provides %a"
          Modname.pp modname
    | Invalid_argument msg | Failure msg -> error_msgf "%s" msg
    | exn -> error_msgf "%s" (Printexc.to_string exn)
  in
  match result with
  | Ok () -> 0
  | Error (`Msg msg) ->
      Fmt.epr "%s: %s\n%!" Filename.(basename Sys.executable_name) msg;
      1

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
    "Exclude a file, or a directory (and its sub-directories), from resolution."
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

let setup_solver without_stdlib recurse exclude ignore forbid =
  let ignore = List.concat ignore in
  let forbid = List.concat forbid in
  Uniq_solver.config ~stdlib:(not without_stdlib) ~recurse ~exclude ~ignore
    ~forbid ()

let setup_solver =
  let open Term in
  const setup_solver $ without_stdlib $ recurse $ exclude $ ignore $ forbid

let term =
  let open Term in
  const run
  $ setup_logs
  $ setup_ocaml
  $ setup_ocamlfind
  $ setup_solver
  $ setup_policy
  $ dirs

let cmd =
  let doc = "Infer packages an OCaml project should vendor." in
  let man =
    [
      `S Manpage.s_description
    ; `P
        "$(tname) scans the given directories for OCaml sources and objects, \
         computes the set of modules the project requires and searches which \
         $(b,ocamlfind) packages provide them. Then, $(tname) prints the \
         $(b,opam) packages from which these $(b,ocamlfind) packages come."
    ; `P
        "The aim is to inform the user about what should be recompiled (or \
         vendored) if they wish to build the project with a different \
         toolchain, such as the one provided by Solo5 (see the \
         $(b,--toolchain) option)."
    ; `P
        "When several $(b,ocamlfind) packages provide the same module, \
         $(tname) asks the user to pick one. These choices can be given in \
         advance with the $(b,--prefer) option or with a policy file (see the \
         $(b,--config) option)."; `S Manpage.s_examples
    ; `P
        "Infer the $(b,opam) packages needed by a unikernel, where the \
         $(b,bin/) directory contains an executable which should not be \
         compiled with the Solo5 toolchain and where $(b,Documents) is a \
         generated module:"
    ; `Pre
        "  \\$ unic infer -r . --toolchain solo5 --exclude bin/ \\\\\n\
        \      --ignore Documents --prefer digestif.c"
    ]
  in
  let info = Cmd.info "infer" ~doc ~man in
  Cmd.v info term
