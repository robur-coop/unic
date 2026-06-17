let setup () =
  OpamFormatConfig.init ();
  OpamCoreConfig.init ();
  let root = OpamStateConfig.opamroot () in
  let _conf = OpamStateConfig.load_defaults ~lock_kind:`Lock_none root in
  ignore _conf

let opam_packages_of_meta_dirs ~switch_state meta_dirs =
  let root = switch_state.OpamStateTypes.switch_global.OpamStateTypes.root in
  let switch = switch_state.OpamStateTypes.switch in
  let cfg = switch_state.OpamStateTypes.switch_config in
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
    switch_state.OpamStateTypes.installed OpamPackage.Name.Set.empty
