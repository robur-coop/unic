(* NOTE(dinosaure): Just to be clear: the goal here is definitely not to use the
   OPAM solver! We should have enough information from our [Uniq_info.t] and
   [codept] files to resolve the dependencies. *)

type unlocked = OpamStateTypes.unlocked
type 'a sw = 'a OpamStateTypes.switch_state

let setup () =
  OpamFormatConfig.init ();
  OpamCoreConfig.init ~safe_mode:true ();
  let root = OpamStateConfig.opamroot () in
  let _cfg = OpamStateConfig.load_defaults ~lock_kind:`Lock_none root in
  ignore _cfg

let with_switch_state f =
  setup ();
  match
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    OpamSwitchState.with_ `Lock_none gt f
  with
  | result -> Ok result
  | exception exn -> Error (`Msg (Printexc.to_string exn))

let libdir ~sw name =
  let root = sw.OpamStateTypes.switch_global.OpamStateTypes.root in
  let switch = sw.OpamStateTypes.switch in
  let cfg = sw.OpamStateTypes.switch_config in
  Fpath.v
    (OpamFilename.Dir.to_string (OpamPath.Switch.lib root switch cfg name))

let package_of_name ~sw name =
  OpamPackage.Set.find_opt
    (fun p -> OpamPackage.Name.equal (OpamPackage.name p) name)
    sw.OpamStateTypes.installed

let depends ~sw name =
  let installed = OpamPackage.names_of_packages sw.OpamStateTypes.installed in
  match package_of_name ~sw name with
  | None -> OpamPackage.Name.Set.empty
  | Some pkg ->
      let opam = OpamSwitchState.opam sw pkg in
      (* Keep only run-time dependencies: test/build/doc/dev deps (e.g. [odoc],
         [alcotest]) are not linked into the project/executable/unikernel. *)
      let formula =
        OpamFilter.filter_deps ~build:false ~post:false ~test:false ~doc:false
          ~dev_setup:false ~dev:false ~default:true
          (OpamFile.OPAM.depends opam)
      in
      let deps =
        OpamFormula.fold_left
          (fun acc (n, _) -> OpamPackage.Name.Set.add n acc)
          OpamPackage.Name.Set.empty formula
      in
      OpamPackage.Name.Set.inter deps installed

let opam_packages_of_meta_dirs ~sw meta_dirs =
  let root = sw.OpamStateTypes.switch_global.OpamStateTypes.root in
  let switch = sw.OpamStateTypes.switch in
  let cfg = sw.OpamStateTypes.switch_config in
  let meta_dir_strs =
    List.map (fun p -> Fpath.to_string (Fpath.to_dir_path p)) meta_dirs
  in
  OpamPackage.Set.fold
    (fun pkg acc ->
      let name = OpamPackage.name pkg in
      let lib_dir = OpamPath.Switch.lib root switch cfg name in
      let lib_s = OpamFilename.Dir.to_string lib_dir ^ "/" in
      let matches =
        List.exists
          (fun meta_s ->
            String.length meta_s >= String.length lib_s
            && String.sub meta_s 0 (String.length lib_s) = lib_s
            || meta_s = lib_s)
          meta_dir_strs
      in
      if matches then OpamPackage.Name.Set.add name acc else acc)
    sw.OpamStateTypes.installed OpamPackage.Name.Set.empty

let package_names_of_meta_dirs ~sw meta_dirs =
  opam_packages_of_meta_dirs ~sw meta_dirs
  |> OpamPackage.Name.Set.elements
  |> List.map OpamPackage.Name.to_string
