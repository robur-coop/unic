open Cmdliner

let default =
  let open Term in
  ret (const (`Help (`Pager, None)))

let () =
  let doc = "$(tname)" in
  let man = [] in
  let info = Cmd.info "unic" ~doc ~man in
  let cmd = Cmd.group ~default info [ Unic_infer.cmd; Unic_info.cmd ] in
  Cmd.(exit (eval' cmd))
