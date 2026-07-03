open Cmdliner

let default =
  let open Term in
  ret (const (`Help (`Pager, None)))

let () =
  let doc = "Analyse the dependencies of an OCaml project." in
  let man =
    [
      `S Manpage.s_description
    ; `P
        "$(tname) analyses an OCaml project (without relying on any build \
         system) and constructs a dependency graph for that project within a \
         given context (such as the one provided by $(b,opam)). From this \
         graph, it is possible to identify which $(b,ocamlfind) packages \
         provide the modules used by the project and which $(b,opam) packages \
         should be recompiled if the user wishes to build the project with a \
         different toolchain (see $(b,unic infer))."
    ; `P
        "$(tname) also offers some tools to inspect OCaml objects (see \
         $(b,unic info) and $(b,unic digest)), to qualify and resolve the \
         dependencies of a project (see $(b,unic qualify) and $(b,unic \
         resolve)), to inspect META files (see $(b,unic meta)) and to print \
         the configuration of an OCaml toolchain (see $(b,unic cfg))."
    ; `S Manpage.s_bugs
    ; `P "Report bugs to <https://github.com/robur-coop/unic/issues>."
    ]
  in
  let info = Cmd.info "unic" ~doc ~man in
  let cmd =
    Cmd.group ~default info
      [
        Unic_infer.cmd; Unic_info.cmd; Unic_qualify.cmd; Unic_meta.cmd
      ; Unic_digest.cmd; Unic_resolve.cmd; Unic_cfg.cmd
      ]
  in
  Cmd.(exit (eval' cmd))
