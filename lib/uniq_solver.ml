let src = Logs.Src.create "uniq.solver"

module Log = (val Logs.src_log src : Logs.LOG)
module MSet = Set.Make (Modname)
module Info = Uniq_info
module Meta = Uniq_meta

let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt
let somef fmt = Fmt.kstr (fun str -> Some str) fmt
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

exception Ambiguous of Modname.t * Meta.Path.t list

let () =
  Printexc.register_printer @@ function
  | Ambiguous (m, []) -> somef "No package provides %a" Modname.pp m
  | Ambiguous (m, pkgs) ->
      somef "Ambiguous module %a (@[<hov>%a@])" Modname.pp m
        Fmt.(Dump.list Meta.Path.pp)
        pkgs
  | _ -> None

type disambiguate = Modname.t -> Meta.Path.t list -> Meta.Path.t

let fail_on_ambiguity : disambiguate = fun m pkgs -> raise (Ambiguous (m, pkgs))

type state = {
    roots: Fpath.t list
  ; policy: Uniq_policy.t
  ; disambiguate: disambiguate
  ; choices: Meta.Path.t Modname.Map.t
  ; committed: Meta.Path.Set.t
}

(* NOTE(dinosaure): the goal of [committed] is to keep some packages we already
   chosen and re-use it when they are a part of possibilities for another
   unqualified module. *)

let crc_of ~roots pkg modname =
  let ( let* ) = Result.bind in
  begin
    let* descrs = Meta.search ~roots pkg in
    let* infos = Meta.to_artifacts descrs in
    let crc = List.find_map (fun v -> Info.crc_of v modname) infos in
    Stdlib.Option.to_result ~none:(msgf "") crc
  end
  |> Result.to_option

let exports_crc state pkg modname crc =
  match (crc_of ~roots:state.roots pkg modname, crc) with
  | Some a, Some b -> Uniq_digest.equal a b
  | _ -> false

module Config = struct
  type t = {
      stdlib: bool
    ; recurse: bool
    ; exclude: Fpath.t list
    ; ignore: MSet.t
    ; forbid: MSet.t
    ; roots: Fpath.t list
    ; policy: Uniq_policy.t
  }

  let cfg ?(stdlib = true) ?(recurse = false) ?(exclude = []) ?(ignore = [])
      ?(forbid = []) ?(policy = Uniq_policy.empty) roots =
    let ignore = MSet.of_list ignore in
    let forbid = MSet.of_list forbid in
    { stdlib; recurse; exclude; ignore; forbid; policy; roots }
end

let missing_modules infos =
  let fn acc t =
    let intfs, impls = Info.missing t in
    let intfs = List.map fst intfs and impls = List.map fst impls in
    let intfs = List.to_seq intfs and impls = List.to_seq impls in
    acc |> MSet.add_seq impls |> MSet.add_seq intfs
  in
  List.fold_left fn MSet.empty infos |> MSet.elements

let commit state pkg =
  let committed = Meta.Path.Set.add pkg state.committed in
  { state with committed }

let remmember state modname pkg = function
  | [] -> state (* NOTE(dinosaure): we decided something /ex-nihilo/ *)
  | _ -> { state with choices= Modname.Map.add modname pkg state.choices }

let decide (state : state) modname crc pkgs =
  let ( let* ) x fn = match x with Some _ as value -> value | None -> fn () in
  match Modname.Map.find_opt modname state.choices with
  | Some pkg -> (state, Some pkg)
  | None -> begin
      let pick =
        match pkgs with
        | [ pkg ] -> Some pkg
        | [] -> None
        | pkgs ->
            let fn pkg = Meta.Path.Set.mem pkg state.committed in
            let* () = List.find_opt fn pkgs in
            let fn pkg = exports_crc state pkg modname crc in
            let* () = List.find_opt fn pkgs in
            Uniq_policy.disambiguate_with state.policy modname pkgs
      in
      match pick with
      | Some pkg ->
          let state = commit state pkg in
          let state = remmember state modname pkg pkgs in
          (state, Some pkg)
      | None -> (state, None)
    end

let decide_or_fail state modname crc pkgs =
  match (decide state modname crc pkgs, pkgs) with
  | (state, Some pkg), _ -> (state, pkg)
  | (_state, None), [] -> raise (Ambiguous (modname, []))
  | (state, None), pkgs ->
      let pkg = state.disambiguate modname pkgs in
      let state = commit state pkg in
      let state = remmember state modname pkg pkgs in
      (state, pkg)

let absolute =
  (* NOTE(dinosaure): [Fpath.v] should be fine! *)
  let cwd = Fpath.v (Sys.getcwd ()) in
  fun path ->
    let path = if Fpath.is_rel path then Fpath.(cwd // path) else path in
    Fpath.normalize path

let step0 ~cfg (state : state) dirs =
  let ( let* ) = Result.bind in
  let fn =
    Uniq_resolve.Src.sources ~recurse:cfg.Config.recurse
      ~exclude:cfg.Config.exclude
  in
  let dirs = List.map absolute dirs in
  let dirs = List.map Fpath.to_dir_path dirs in
  let srcs = List.map fn dirs in
  Log.debug (fun m ->
      m "qualify (with stdlib:%b): @[<hov>%a@]" cfg.Config.stdlib
        Fmt.(list ~sep:(any ";@ ") Uniq_resolve.Src.pp)
        srcs);
  let* infos = Uniq_resolve.qualify ~stdlib:cfg.Config.stdlib srcs in
  Log.debug (fun m ->
      m "qualified: @[<hov>%a@]" Fmt.(list ~sep:(any ";@ ") Info.pp) infos);
  let missing = missing_modules infos in
  Log.debug (fun m ->
      m "missing: @[<hov>%a@]" Fmt.(list ~sep:(any ";@ ") Modname.pp) missing);
  let* () =
    let fn m = MSet.mem m cfg.forbid in
    match List.filter fn missing with
    | [] -> Ok ()
    | ms ->
        error_msgf "Forbidden module(s) referenced: @[<hov>%a@]"
          Fmt.(list ~sep:(any ",@ ") Modname.pp)
          ms
  in
  let pkgs = Meta.find_providers ~roots:state.roots missing in
  let fn (m, pkgs) = match pkgs with [] -> None | pkgs -> Some (m, pkgs) in
  let pkgs = List.filter_map fn pkgs in
  let fn acc (m, _pkgs) = MSet.add m acc in
  let mods = List.fold_left fn MSet.empty pkgs in
  let fn m = (not (MSet.mem m mods)) && not (MSet.mem m cfg.Config.ignore) in
  let missing = List.filter fn missing in
  let* () =
    match missing with
    | [] -> Ok ()
    | ms ->
        error_msgf
          "No package provides @[<hov>%a@]; pass --ignore for generated or \
           intentionally-absent modules"
          Fmt.(list ~sep:(any ",@ ") Modname.pp)
          ms
  in
  let rec soft state pending =
    let fn (state, pending, progress) (m, pkgs) =
      match decide state m None pkgs with
      | state, Some _ -> (state, pending, true)
      | state, None -> (state, (m, pkgs) :: pending, progress)
    in
    let state, pending, progressed =
      List.fold_left fn (state, [], false) pending
    in
    if progressed then soft state pending else (state, pending)
  in
  let rec force state = function
    | [] -> state
    | (m, pkgs) :: pending ->
        let state =
          match decide state m None pkgs with
          | state, Some _ -> state
          | state, None -> fst (decide_or_fail state m None pkgs)
        in
        let state, pending = soft state pending in
        force state pending
  in
  (* NOTE(dinosaure): here, we start with a /soft/ resolution of direct
     dependencies. Throughout the resolution process, we record the choices (in
     accordance with our "policy") that we have made, in the hope that this
     might help subsequent iterations to resolve ambiguities without user
     intervention.

     We then we /force/ the resolution, and this time the system may potentially
     ask the user for information ([digestif.c] or [digestif.ocaml]?). This
     process will only end once all direct dependencies have been resolved or
     when we have failed to resolve an ambiguity. *)
  let state, rem = soft state pkgs in
  let state = force state rem in
  Ok (state, Meta.Path.Set.elements state.committed)

let step0 ~cfg (state : state) dirs =
  match step0 ~cfg state dirs with
  | (Ok _ | Error _) as value -> value
  | exception Ambiguous (m, []) ->
      error_msgf "No package provides %a" Modname.pp m
  | exception Ambiguous (m, pkgs) ->
      error_msgf "Ambiguous module %a (@[<hov>%a@])" Modname.pp m
        Fmt.(Dump.list Meta.Path.pp)
        pkgs

let artifacts ~roots pkg =
  match Meta.search ~roots pkg with
  | Error _ | Ok [] -> None
  | Ok [ ((dirpath, _) as descr) ] ->
      let objs = Meta.to_artifacts [ descr ] in
      let objs = Result.value ~default:[] objs in
      Some (dirpath, objs)
  | Ok _ -> Fmt.failwith "Multiple solution for %a" Meta.Path.pp pkg

let is_stdlib m =
  let m = Modname.to_string m in
  (* TODO(dinosaure): we probably can "flatten" [Bundle.stdlib] from [codept]
     to discriminate strictly any modules from the standard library. Actually,
     [codept] defines [Bundle.stdlib] as a [Namespace]. *)
  let prefixes = [ "Stdlib"; "Stdlib__"; "Camlinternal"; "Std_exit" ] in
  List.exists (fun prefix -> String.starts_with ~prefix m) prefixes

let self objs =
  let set = Hashtbl.create 0x7ff in
  let fn (mpath, _) =
    match Info.Path.to_list mpath with
    | [ m ] -> Hashtbl.replace set m ()
    | _ -> ()
  in
  let fn obj = List.iter fn (Info.exports obj) in
  List.iter fn objs; set

let deps objs =
  let self = self objs in
  let fn0 obj =
    let crcs = Hashtbl.create 0x7ff in
    let fn (m, crc) = Hashtbl.replace crcs m crc in
    List.iter fn (Info.intfs_imported obj);
    List.iter fn (Info.impls_imported obj);
    let intfs, impls = Info.missing obj in
    let fn modname =
      let crc = Hashtbl.find_opt crcs modname in
      (modname, Option.join crc)
    in
    let intfs = List.map fst intfs in
    let impls = List.map fst impls in
    let deps0 = List.map fn intfs in
    let deps1 = List.rev_map fn impls in
    List.rev_append deps1 deps0
  in
  let fn1 (m, _crc) = (not (is_stdlib m)) && not (Hashtbl.mem self m) in
  let deps = List.concat_map fn0 objs |> List.filter fn1 in
  let pp_elt ppf (modname, crc) =
    Fmt.pf ppf "%a(%a)" Modname.pp modname
      Fmt.(option ~none:(any "<none>") Uniq_digest.pp)
      crc
  in
  Logs.debug (fun m ->
      m "dependencies for %a: @[<hov>%a@]"
        Fmt.(list ~sep:(any ",") (using Info.modname Modname.pp))
        objs
        Fmt.(list ~sep:(any ";@ ") pp_elt)
        deps);
  deps

type node = {
    dirpath: Fpath.t
  ; objs: Info.t list
  ; deps: (Meta.Path.t * [ `CRC | `Name ]) list
}

type graph = node Uniq_meta.Path.Map.t

let solve ~cfg state ~resolve directs =
  let dedup =
    let tbl = Hashtbl.create 0x7ff in
    fun imports ->
      let fn (m, _) =
        match Hashtbl.mem tbl m with
        | true -> false
        | false -> Hashtbl.add tbl m (); true
      in
      List.filter fn imports
  in
  let rec go state nodes visited frontier =
    let fn pkg = not (Meta.Path.Set.mem pkg visited) in
    match List.filter fn frontier with
    | [] -> (state, nodes)
    | frontier ->
        Log.debug (fun m ->
            m "frontier: @[<hov>%a@]"
              Fmt.(list ~sep:(any ";@ ") Meta.Path.pp)
              frontier);
        let fn acc pkg = Meta.Path.Set.add pkg acc in
        let visited = List.fold_left fn visited frontier in
        let fn pkg =
          match artifacts ~roots:cfg.Config.roots pkg with
          | None -> None
          | Some (dirpath, objs) -> Some (pkg, dirpath, objs, deps objs)
        in
        let entries = List.filter_map fn frontier in
        let fn (_, _, _, deps) = deps in
        let imports = dedup (List.concat_map fn entries) in
        let state, pkgs' = resolve state imports in
        let pkgs' =
          let tbl = Hashtbl.create 0x7ff in
          List.iter (fun (m, pkg) -> Hashtbl.replace tbl m pkg) pkgs';
          tbl
        in
        let fn (nodes, directs') (pkg, dirpath, objs, deps) =
          let links = Hashtbl.create 0x7ff in
          let fn (modname, crc) =
            match Hashtbl.find_opt pkgs' modname with
            | Some pkg' when Meta.Path.equal pkg pkg' = false ->
                let lnk = if Stdlib.Option.is_some crc then `CRC else `Name in
                let cur = Hashtbl.find_opt links pkg' in
                let lnk = match cur with Some (_, `CRC) -> `CRC | _ -> lnk in
                Hashtbl.replace links pkg' (pkg', lnk)
            | _ -> ()
          in
          List.iter fn deps;
          let deps =
            Hashtbl.fold (fun _ v acc -> v :: acc) links []
            |> List.sort_uniq (fun (a, _) (b, _) -> Meta.Path.compare a b)
          in
          let elt = { dirpath; objs; deps } in
          let nodes = Meta.Path.Map.add pkg elt nodes in
          let fn acc pkg = Meta.Path.Set.add pkg acc in
          let directs' = List.fold_left fn directs' (List.map fst deps) in
          (nodes, directs')
        in
        let nodes, directs' =
          List.fold_left fn (nodes, Meta.Path.Set.empty) entries
        in
        go state nodes visited (Meta.Path.Set.to_list directs')
  in
  go state Meta.Path.Map.empty Meta.Path.Set.empty directs

(* TODO(dinosaure): we have a special case for the stdlib. The question is: why
   we don't pick [Stdlib__*] (or [stdlib.cmxa]) from what the [META] gives to
   us? We should check that we interpret, as [ocamlfind], correctly the given
   [META] file from the OCaml distribution. *)

let verify ~cfg g =
  let exports = Hashtbl.create 0x7ff in
  let fn2 (mpath, _) =
    match Info.Path.to_list mpath with
    | [ m ] -> Hashtbl.replace exports m ()
    | _ -> ()
  in
  let fn1 info = List.iter fn2 (Info.exports info) in
  let fn0 _ node = List.iter fn1 node.objs in
  Meta.Path.Map.iter fn0 g;
  let candidates = Hashtbl.create 0x7ff in
  let fn2 modname =
    if not (Hashtbl.mem exports modname) then
      Hashtbl.replace candidates modname ()
  in
  let fn1 info =
    let intfs, impls = Info.missing info in
    let intfs = List.map fst intfs in
    let impls = List.map fst impls in
    List.iter fn2 (intfs @ impls)
  in
  let fn0 _ node = List.iter fn1 node.objs in
  Meta.Path.Map.iter fn0 g;
  let candidates = Hashtbl.fold (fun m () acc -> m :: acc) candidates [] in
  let fn acc (modname, pkgs) =
    match pkgs with [] -> acc | _ -> MSet.add modname acc
  in
  let providers = Meta.find_providers ~roots:cfg.Config.roots candidates in
  let provided = List.fold_left fn MSet.empty providers in
  let fn m = (not (is_stdlib m)) && not (MSet.mem m provided) in
  match List.filter fn candidates with
  | [] -> Ok ()
  | missing ->
      error_msgf
        "Impossible to statically link your program (missing module(s): \
         @[<hov>%a@])"
        Fmt.(Dump.list Modname.pp)
        missing

let solve ~cfg ?(disambiguate = fail_on_ambiguity) dirs =
  let ( let* ) = Result.bind in
  let state =
    {
      roots= cfg.Config.roots
    ; policy= cfg.Config.policy
    ; disambiguate
    ; choices= Modname.Map.empty
    ; committed= Meta.Path.Set.empty
    }
  in
  let* state, directs = step0 ~cfg state dirs in
  (* NOTE(dinosaure): about the empty case for [modules], due to [dedup] into
     our solver, it's possible that we already picked a pkg for a specific
     modules and choose it instead of try to find a new provider. So we actually
     can ask an empty list of [modules] to our resolver and we just need to
     return an empty list of packages. *)
  let resolve state = function
    | [] -> (state, [])
    | modules ->
        let fn acc (m, crc) = Modname.Map.add m crc acc in
        let crc_of = List.fold_left fn Modname.Map.empty modules in
        let state, acc =
          let modules = List.map fst modules in
          let pkgs = Meta.find_providers ~roots:state.roots modules in
          let fn (state, acc) (modname, pkgs) =
            let crc = Option.join (Modname.Map.find_opt modname crc_of) in
            match pkgs with
            | [] -> (state, acc)
            | pkgs ->
                let state, pkg = decide_or_fail state modname crc pkgs in
                (state, (modname, pkg) :: acc)
          in
          List.fold_left fn (state, []) pkgs
        in
        (state, List.rev acc)
  in
  let _state, g = solve ~cfg state ~resolve directs in
  let* () = verify ~cfg g in
  Ok g

module Ng = struct
  type cfg = {
      stdlib: bool
    ; recurse: bool
    ; exclude: Fpath.t list
    ; forbid: MSet.t
  }

  let config ?(stdlib = true) ?(recurse = true) ?(exclude = []) ?(forbid = [])
      () =
    let forbid = MSet.of_list forbid in
    { stdlib; recurse; exclude; forbid }

  type providers = ?crc:Digest.t -> Modname.t -> Info.t option

  let absolute =
    (* NOTE(dinosaure): [Fpath.v] should be fine! *)
    let cwd = Fpath.v (Sys.getcwd ()) in
    fun path ->
      let path = if Fpath.is_rel path then Fpath.(cwd // path) else path in
      Fpath.normalize path

  exception
    Impossible_to_requalify of
      Info.t * Fpath.t * Digest.t option * [ `Intf | `Impl ] * Modname.t

  (* NOTE(dinosaure): we try to solve missing modules from what we already have.
     For instance, a [*.ml] can requires [x509] but we also have [x509.cmi] as
     an artifact into our [infos]:
     - [x509.cmi] ∈ [infos]
     - & [(X509, _)] ∈ [modules] => one of our artifacts requires [X509]

     In that case, that mostly means that one of our [infos] artifact told to us
     that it requires [X509] (see [missing_intfs]). Here, we will try to
     consolidate our [infos] and requalify some of our artifacts with our
     [x509.cmi] instead of asking the caller a new provider for [X509].

     It take into account only [exports] modules! The objective here is to
     qualify dependencies on our [info] with [Fully_qualified]. About [Located],
     it must be done in another pass when we can not any find solutions from
     what [*.cmi]/[*.mli] exports. *)
  let prune infos modules =
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
    let fn0 (infos, progress, rem) (m, crc) =
      match List.filter_map (fn1 (m, crc)) infos with
      | [ solution ] ->
          Log.debug (fun mf ->
              mf "take %a for %a" Info.pp solution Modname.pp m);
          let location = Info.location solution in
          let fn info =
            match Info.qualify info ~location ?crc `Intf m with
            | Some info -> info
            | None ->
                raise (Impossible_to_requalify (info, location, crc, `Intf, m))
          in
          let infos = List.map fn infos in
          (infos, true, rem)
      | _ :: _ as solutions ->
          Log.err (fun mf ->
              mf "Multiple solutions for %a (%a)" Modname.pp m
                Fmt.(option ~none:(any "<none>") Uniq_digest.pp)
                crc);
          Log.err (fun m ->
              m "Actual artifacts: @[<hov>%a@]"
                Fmt.(list ~sep:(any ",@ ") Info.pp)
                infos);
          Log.err (fun m ->
              m "Solutions: @[<hov>%a@]"
                Fmt.(list ~sep:(any ",@ ") Info.pp)
                solutions);
          assert false
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
    if List.exists (fun (m, _) -> MSet.mem m cfg.forbid) modules = false then
      Ok ()
    else error_msgf "Foo"

  let sort = List.sort_uniq (fun (a, _) (b, _) -> Modname.compare a b)
  let dummy = String.make (Uniq_digest.length * 2) '-'

  let solve_intfs ~cfg:({ recurse; exclude; stdlib; _ } as cfg) ~providers dirs
      =
    let ( let* ) = Result.bind in
    let fn = Uniq_resolve.Src.sources ~recurse ~exclude in
    let dirs = List.map absolute dirs in
    let dirs = List.map Fpath.to_dir_path dirs in
    let srcs = List.map fn dirs in
    let fn (infos, progress, rem) (m, crc) =
      Log.debug (fun mf ->
          mf "ask for %a (%a)" Modname.pp m
            Fmt.(option ~none:(const (fmt "%s") dummy) Uniq_digest.pp)
            crc);
      begin match providers ?crc m with
      | None -> (infos, progress, (m, crc) :: rem)
      | Some solution when List.exists (Info.equal solution) infos = false ->
          let location = Info.location solution in
          (* NOTE(dinosaure): here, [crc] is still the one from what it miss
             but, with the solution, we can actually fix it with what [solution]
             gives to us. We can observe some requalification from [Location] to
             [Fully_qualified] which is fine but I suspect an override of the
             [crc] sometimes... *)
          let fn info =
            match Info.qualify info ~location ?crc `Intf m with
            | Some info -> info
            | None ->
                raise (Impossible_to_requalify (info, location, crc, `Intf, m))
          in
          let infos = List.map fn infos in
          Log.debug (fun m -> m "add %a" Info.pp solution);
          (solution :: infos, true, rem)
      | Some _solution -> assert false
      end
    in
    let rec go infos =
      match missing_intfs infos |> sort with
      | [] -> Ok (infos, [])
      | modules ->
          Log.debug (fun m ->
              m "search (00): @[<hov>%a@]"
                Fmt.(list ~sep:(any ",@ ") Modname.pp)
                (List.map fst modules));
          let* () = not_in_forbidden_modules cfg modules in
          let infos, modules = prune infos modules in
          let modules = sort modules in
          Log.debug (fun m ->
              m "search (01): @[<hov>%a@]"
                Fmt.(list ~sep:(any ",@ ") Modname.pp)
                (List.map fst modules));
          let infos, progress, modules =
            match modules with
            | [] -> (infos, true, [])
            | modules -> List.fold_left fn (infos, false, []) modules
          in
          if progress then go infos else Ok (infos, modules)
    in
    let* infos = Uniq_resolve.qualify ~stdlib srcs in
    go infos
end
