let src = Logs.Src.create "uniq.info"
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

module Log = (val Logs.src_log src : Logs.LOG)
module Digest = Uniq_digest

module Path : sig
  type t

  val to_list : t -> Modname.t list
  val of_list : Modname.t list -> t
  val singleton : Modname.t -> t
  val pp : t Fmt.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val prepend : Modname.t -> t -> t
  val is_a_part : part:t -> t -> bool

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end = struct
  type t = Modname.t list

  let of_list = function
    | [] -> invalid_arg "Uniq_info.Path.of_list"
    | lst -> lst

  let to_list x = x
  let singleton x = [ x ]
  let prepend m t = m :: t

  let compare lst0 lst1 =
    let len0 = List.length lst0 and len1 = List.length lst1 in
    let res0 = Int.compare len0 len1 in
    if res0 == 0 then begin
      let res1 = ref 0 in
      let exception Stop in
      let fn m0 m1 =
        res1 := Modname.compare m0 m1;
        if !res1 != 0 then raise Stop
      in
      try List.iter2 fn lst0 lst1; 0 with Stop -> !res1
    end
    else res0

  let equal a b = compare a b = 0
  let pp = Fmt.list ~sep:(Fmt.any ".") Modname.pp

  let is_a_part ~part t =
    let rec check a b =
      match (a, b) with
      | [], _ -> true
      | x :: r, x' :: r' ->
          if Modname.compare x x' = 0 then check r r' else find part r'
      | _ :: _, [] -> false
    and find a b =
      match (a, b) with
      | x :: r, x' :: r' ->
          if Modname.compare x x' = 0 then check r r' else find part r'
      | [], _ -> assert false
      | _ :: _, [] -> false
    in
    find part t

  module Set = Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  module Map = Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end

[@@@warning "-32"]

module RPath : sig
  type t

  val extend : modname:Modname.t -> t -> t
  val empty : t
  val singleton : Modname.t -> t
  val to_path : t -> Path.t
  val pp : t Fmt.t

  module Set : Set.S with type elt = t
end = struct
  type t = Modname.t list

  let extend ~modname v = modname :: v
  let empty = []
  let singleton modname = [ modname ]

  let to_path = function
    | [] -> invalid_arg "RPath.to_path"
    | lst -> Path.of_list (List.rev lst)

  let pp = Fmt.(Dump.list Modname.pp)

  module Set = Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end

type t = {
    name: Unitname.t
  ; version: int option
  ; exports: (Modname.t * Digest.t option) list
  ; modules: Path.Set.t
  ; intfs: elt list
  ; impls: elt list
  ; format: format
}

and elt =
  | Qualified of Modname.t * Digest.t
  | Fully_qualified of Modname.t * Digest.t * Fpath.t
  | Located of Modname.t * Fpath.t
  | Named of Modname.t

and 'a kind =
  | Ml : Comp_unit.u kind
  | Mli : Comp_unit.u kind
  | Cmo : Cmo_format.compilation_unit kind
  | Cma : Cmo_format.library kind
  | Cmi : Cmi_format.cmi_infos kind
  | Cmx : Cmx_format.unit_infos kind
  | Cmxa : Cmx_format.library_infos kind

and format = Format : 'a kind * 'a -> format

let pp ppf t = Fmt.string ppf (Unitname.filepath t.name)

let pp_elt ppf = function
  | Qualified (modname, crc) ->
      Fmt.pf ppf "%a(%a)" Modname.pp modname Uniq_digest.pp crc
  | Fully_qualified (_, crc, path) ->
      Fmt.pf ppf "%a(%a)" Fpath.pp path Uniq_digest.pp crc
  | Located (_, path) -> Fpath.pp ppf path
  | Named modname -> Modname.pp ppf modname

let equal a b =
  String.equal (Unitname.filepath a.name) (Unitname.filepath b.name)

exception Inconsistency of Unitname.t * Modname.t * Digest.t * Digest.t

let inconsistency location name crc crc' =
  let unit = Unitname.modulize (Fpath.to_string location) in
  raise (Inconsistency (unit, name, crc, crc'))

let is_fully_resolved t =
  List.for_all (function Fully_qualified _ -> true | _ -> false) t.intfs
  && List.for_all (function Fully_qualified _ -> true | _ -> false) t.impls

let is_a_library t =
  match t.format with
  | Format (Cma, _) -> true
  | Format (Cmxa, _) -> true
  | _ -> false

let has_c_stubs t =
  match t.format with
  | Format (Cmxa, li) -> li.Cmx_format.lib_ccobjs <> []
  | Format (Cma, toc) -> toc.Cmo_format.lib_ccobjs <> []
  | _ -> false

(* NOTE(dinosaure): collect [-L] paths. *)
let c_library_dirs t =
  let ccflags =
    match t.format with
    | Format (Cmxa, li) -> li.Cmx_format.lib_ccobjs @ li.Cmx_format.lib_ccopts
    | Format (Cma, toc) -> toc.Cmo_format.lib_ccobjs @ toc.Cmo_format.lib_ccopts
    | _ -> []
  in
  let rec collect acc = function
    | [] -> acc
    | tok :: rest when String.length tok > 2 && String.sub tok 0 2 = "-L" ->
        collect (String.sub tok 2 (String.length tok - 2) :: acc) rest
    | "-L" :: dir :: rest -> collect (dir :: acc) rest
    | _ :: rest -> collect acc rest
  in
  let tokens =
    List.concat_map (String.split_on_char ' ') ccflags
    |> List.concat_map (String.split_on_char '\t')
    |> List.filter (fun s -> s <> "")
  in
  collect [] tokens
  |> List.filter_map (fun s -> Result.to_option (Fpath.of_string s))
  |> List.sort_uniq Fpath.compare

let is_native t =
  match t.format with
  | Format (Cmx, _) -> true
  | Format (Cmxa, _) -> true
  | Format (Cmi, _) -> true
  | _ -> false

let is_an_interface t =
  match t.format with
  | Format (Mli, _) -> true
  | Format (Cmi, _) -> true
  | _ -> false

let is_a_cmi t = match t.format with Format (Cmi, _) -> true | _ -> false

let of_elt = function
  | Qualified (m, crc) -> (m, Some crc)
  | Fully_qualified (m, crc, _) -> (m, Some crc)
  | Located (m, _) -> (m, None)
  | Named m -> (m, None)

let exports t =
  let inner = Path.Set.to_list t.modules in
  let inner = List.map (fun t -> (t, None)) inner in
  let outer = List.map (fun (m, d) -> (Path.singleton m, d)) t.exports in
  List.rev_append outer inner

let location t = Fpath.v (Unitname.filepath t.name)
let intfs_imported t = List.map of_elt t.intfs
let impls_imported t = List.map of_elt t.impls
let modname t = Unitname.modname t.name

let missing t =
  let fn = function
    | Named m -> Either.Left (m, None)
    | Qualified (m, crc) -> Either.Left (m, Some crc)
    | _ -> Either.Right ()
  in
  let intfs, _ = List.partition_map fn t.intfs in
  let impls, _ = List.partition_map fn t.impls in
  let fn1 modname crc (modname', crc') =
    match (crc, crc') with
    | None, None -> Modname.compare modname modname' = 0
    | Some crc, Some crc' when Modname.compare modname modname' = 0 ->
        if not (Digest.equal crc crc') then
          raise (Inconsistency (t.name, modname, crc, crc'));
        true
    | Some _, Some _ -> false
    | None, Some _ | Some _, None -> Modname.compare modname modname' = 0
  in
  let fn0 (modname, crc) = not (List.exists (fn1 modname crc) t.exports) in
  let intfs =
    match t.format with
    | Format (Cmi, _) | Format (Mli, _) -> List.filter fn0 intfs
    | _ -> intfs
  in
  let impls =
    match t.format with
    | Format (Cmo, _) | Format (Cma, _) | Format (Cmx, _) | Format (Cmxa, _) ->
        List.filter fn0 impls
    | _ -> impls
  in
  (intfs, impls)

let crc_of t m =
  let ( >>| ) x f = Stdlib.Option.map f x in
  List.find_opt (fun (m', _) -> Modname.compare m m' = 0) t.exports
  >>| snd
  |> Option.join

let kind t =
  match t.format with
  | Format (Ml, _)
  | Format (Cmo, _)
  | Format (Cma, _)
  | Format (Cmx, _)
  | Format (Cmxa, _) ->
      `Impl
  | Format (Mli, _) | Format (Cmi, _) -> `Intf

let elt_name = function
  | Qualified (x, _) | Fully_qualified (x, _, _) | Named x | Located (x, _) -> x

let elt_replace unitname new_value elts =
  let fn acc old_value =
    let a = elt_name new_value in
    let b = elt_name old_value in
    if Modname.compare a b = 0 then
      match (old_value, new_value) with
      (* promotion [Named -> *] *)
      | Named _, _ -> new_value :: acc
      (* keep best [old_value] against [Named] *)
      | Qualified _, Named _ -> old_value :: acc
      | Fully_qualified _, Named _ -> old_value :: acc
      | Located _, Named _ -> old_value :: acc
      (* promotion [Qualified with Located -> Fully_qualified] *)
      | Qualified (_, crc), Located (_, path) ->
          Fully_qualified (a, crc, path) :: acc
      (* promotion [Qualified -> Fully_qualified] *)
      | Qualified (_, crc), Fully_qualified (_, crc', _) ->
          if Uniq_digest.equal crc crc' then new_value :: acc
          else raise (Inconsistency (unitname, a, crc, crc'))
      (* ignore [Qualified] because we can not check [crc] from [Located] *)
      | Located _, Qualified _ -> old_value :: acc
      (* promotion [Located -> Fully_qualified] iff they refer to the same artifact *)
      | Located (_, path), Fully_qualified (_, _, path') ->
          if Fpath.equal path path' then new_value :: acc
          else old_value :: acc (* TODO(dinosaure): raise or ignore? *)
      (* check [crc] and keep [Fully_qualified] or raise *)
      | Fully_qualified (_, crc, _), Qualified (_, crc') ->
          if Uniq_digest.equal crc crc' then old_value :: acc
          else raise (Inconsistency (unitname, a, crc, crc'))
      (* ignore but we should raise if [path] is not the same *)
      | Fully_qualified _, Located _ -> old_value :: acc
      (* identity or raise if [crc] is not the same *)
      | Qualified (_, crc), Qualified (_, crc') ->
          if Uniq_digest.equal crc crc' then new_value :: acc
          else raise (Inconsistency (unitname, a, crc, crc'))
      (* identity or raise if [crc] is not the same,
         ignore if [path] is not the same (or raise?) *)
      | Fully_qualified (_, crc, path), Fully_qualified (_, crc', path') ->
          if Uniq_digest.equal crc crc' && Fpath.equal path path' then
            new_value :: acc
          else if Uniq_digest.equal crc crc' = false then
            raise (Inconsistency (unitname, a, crc, crc'))
          else old_value :: acc
      (* identity or ignore if [path] is the same *)
      | Located (_, path), Located (_, path') ->
          if Fpath.equal path path' then new_value :: acc else old_value :: acc
    else old_value :: acc
  in
  List.fold_left fn [] elts |> List.rev

let qualify t ?location ?crc kind modname =
  Log.debug (fun m -> m "requalify %a" pp t);
  let elt =
    match (location, crc) with
    | None, Some crc -> Qualified (modname, crc)
    | Some location, Some crc -> Fully_qualified (modname, crc, location)
    | None, None -> Named modname
    | Some location, None -> Located (modname, location)
  in
  match kind with
  | `Intf -> { t with intfs= elt_replace t.name elt t.intfs }
  | `Impl -> { t with impls= elt_replace t.name elt t.impls }

let elt_compare a b =
  let a = elt_name a in
  let b = elt_name b in
  Modname.compare a b

let elt_find modname lst =
  try
    List.find
      (fun elt ->
        let modname' = elt_name elt in
        Modname.compare modname modname' = 0)
      lst
    |> function
    | Qualified (_, crc) -> [ (modname, Some crc) ]
    | Fully_qualified (_, crc, _) -> [ (modname, Some crc) ]
    | Named _ -> [ (modname, None) ]
    | Located _ -> [ (modname, None) ]
  with Not_found -> []

open Bos

let to_elt (str, crc) =
  match crc with
  | Some crc -> Qualified (Modname.v str, crc)
  | None -> Named (Modname.v str)

let collect_modules_on_cmi { Cmi_format.cmi_sign; _ } =
  let rec on_signature_item ~prefix acc = function
    | Types.Sig_module (ident, _, { md_type; _ }, _, _) ->
        let modname = Modname.v (Ident.name ident) in
        let prefix = RPath.extend ~modname prefix in
        begin match md_type with
        | Types.(Mty_ident _ | Mty_functor _ | Mty_alias _) ->
            RPath.Set.add prefix acc
        | Mty_signature s ->
            let fn acc sig_item = on_signature_item ~prefix acc sig_item in
            List.fold_left fn (RPath.Set.add prefix acc) s
        end
    | Types.Sig_value _ | Types.Sig_type _ | Types.Sig_typext _
    | Types.Sig_class _ | Types.Sig_class_type _ ->
        acc
    | Types.Sig_modtype _ -> acc
    (* TODO(dinosaure) *)
  in
  let fn acc sig_item = on_signature_item ~prefix:RPath.empty acc sig_item in
  let set = List.fold_left fn RPath.Set.empty cmi_sign in
  let fn elt acc = Path.Set.add (RPath.to_path elt) acc in
  RPath.Set.fold fn set Path.Set.empty

let info_of_cmi ~location ~version _ic =
  match Cmt_format.read (Fpath.to_string location) with
  | None, _ -> error_msgf "Invalid cmi object: %a" Fpath.pp location
  | Some cmi, _ ->
      let intfs = cmi.Cmi_format.cmi_crcs in
      let intfs = List.map to_elt intfs in
      let intfs = List.sort elt_compare intfs in
      let impls = [] in
      let format = Format (Cmi, cmi) in
      let name = Unitname.modulize (Fpath.to_string location) in
      let exports = elt_find (Unitname.modname name) intfs in
      let modules = collect_modules_on_cmi cmi in
      let modules =
        Path.Set.map (Path.prepend (Unitname.modname name)) modules
      in
      Ok { name; version; modules; exports; intfs; impls; format }
  | exception _ -> error_msgf "Invalid cmi object: %a" Fpath.pp location

let info_of_cmo ~location ~version ic =
  let cu_pos = input_binary_int ic in
  seek_in ic cu_pos;
  let cu = (input_value ic : Cmo_format.compilation_unit) in
  let intfs = List.map to_elt cu.cu_imports in
  let intfs = List.sort elt_compare intfs in
  let impls = [] in
  let format = Format (Cmo, cu) in
  let name = Unitname.modulize (Fpath.to_string location) in
  let exports = [ (Unitname.modname name, None) ] in
  Ok { name; version; modules= Path.Set.empty; exports; intfs; impls; format }

let info_of_cmx ~location ~version ic =
  let ui = (input_value ic : Cmx_format.unit_infos) in
  let name = Unitname.modulize (Fpath.to_string location) in
  let exports = [ (Unitname.modname name, Some (Digest.input ic)) ] in
  let intfs = List.map to_elt ui.ui_imports_cmi in
  let intfs = List.sort elt_compare intfs in
  let impls = List.map to_elt ui.ui_imports_cmx in
  let impls = List.sort elt_compare impls in
  let format = Format (Cmx, ui) in
  Ok { name; version; modules= Path.Set.empty; exports; intfs; impls; format }

let to_elt (modname, crc) =
  match crc with Some crc -> Qualified (modname, crc) | None -> Named modname

let info_of_cma ~location ~version ic =
  let toc_pos = input_binary_int ic in
  seek_in ic toc_pos;
  let toc = (input_value ic : Cmo_format.library) in
  let importss =
    List.map (fun { Cmo_format.cu_imports; _ } -> cu_imports) toc.lib_units
  in
  let fold m (str, crc) =
    let name = Modname.v str in
    match (Modname.Map.find_opt name m, crc) with
    | None, _ -> Modname.Map.add name crc m
    | Some None, _ | Some (Some _), None ->
        Modname.Map.add name crc (Modname.Map.remove name m)
    | Some (Some crc'), Some crc ->
        if crc <> crc' then inconsistency location name crc crc';
        m
  in
  let imports = List.concat importss in
  let m = List.fold_left fold Modname.Map.empty imports in
  let exports =
    List.map
      (fun { Cmo_format.cu_name= Compunit cu_name; _ } ->
        (Modname.v cu_name, None))
      toc.lib_units
  in
  let intfs = Modname.Map.bindings m in
  let intfs = List.map to_elt intfs in
  let impls = [] in
  let format = Format (Cma, toc) in
  let name = Unitname.modulize (Fpath.to_string location) in
  Ok { name; version; modules= Path.Set.empty; exports; intfs; impls; format }

let info_of_cmxa ~location ~version ic =
  let li = (input_value ic : Cmx_format.library_infos) in
  let importss_cmi =
    List.map
      (fun ({ Cmx_format.ui_imports_cmi; _ }, _crc) -> ui_imports_cmi)
      li.lib_units
  in
  let importss_cmx =
    List.map
      (fun ({ Cmx_format.ui_imports_cmx; _ }, _crc) -> ui_imports_cmx)
      li.lib_units
  in
  let fold m (str, crc) =
    let name = Modname.v str in
    match (Modname.Map.find_opt name m, crc) with
    | None, _ -> Modname.Map.add name crc m
    | Some None, _ | Some (Some _), None ->
        Modname.Map.add name crc (Modname.Map.remove name m)
    | Some (Some crc'), Some crc ->
        if crc <> crc' then inconsistency location name crc crc';
        m
  in
  let exports =
    List.map
      (fun ({ Cmx_format.ui_name; _ }, crc) -> (Modname.v ui_name, Some crc))
      li.lib_units
  in
  let m = List.fold_left fold Modname.Map.empty (List.concat importss_cmi) in
  let intfs = Modname.Map.bindings m in
  let intfs = List.map to_elt intfs in
  let m = List.fold_left fold Modname.Map.empty (List.concat importss_cmx) in
  let impls = Modname.Map.bindings m in
  let impls = List.map to_elt impls in
  let format = Format (Cmxa, li) in
  let name = Unitname.modulize (Fpath.to_string location) in
  Ok { name; version; modules= Path.Set.empty; exports; intfs; impls; format }

let is_intf location = Fpath.mem_ext [ ".mli" ] location

let pp_dep ppf { Deps.path; edge; pkg; aliases } =
  Fmt.pf ppf "%s%a(%a)%a"
    (if edge = Deps.Edge.Normal then "" else "ε⋅")
    Namespaced.pp path Pkg.pp pkg Namespaced.Set.pp aliases

let to_elt (intfs, impls) { Deps.path; pkg; _ } =
  let name = Namespaced.module_name path in
  match pkg.source with
  | Pkg.Pkg v ->
      let location = Fpath.v (Namespaced.filepath v) in
      if is_intf location then (Located (name, location) :: intfs, impls)
      else (intfs, Located (name, location) :: impls)
  | Pkg.Local ->
      let location = Fpath.v (Namespaced.filepath pkg.file) in
      if is_intf location then (Located (name, location) :: intfs, impls)
      else (intfs, Located (name, location) :: impls)
  | Pkg.Special _ -> (intfs, impls)
  | _ -> (Named name :: intfs, impls)

let collect_modules_on_mli ~modname m2l =
  let rec on_module_sig ~prefix acc = function
    | M2l.Sig ts ->
        let ts = List.map (fun { Loc.data; _ } -> data) ts in
        List.fold_left (on_expression ~prefix) acc ts
    | _ -> acc
  and on_module_expr ~prefix acc = function
    | M2l.Constraint (_me, ms) -> on_module_sig ~prefix acc ms
    | _ -> acc
  and on_expression ~prefix acc (expr : M2l.expression) =
    match expr with
    | M2l.Bind { name= Some name; expr } ->
        let modname = Modname.v name in
        let prefix = RPath.extend ~modname prefix in
        let acc = RPath.Set.add prefix acc in
        on_module_expr ~prefix acc expr
    | M2l.Bind_rec ms ->
        let fn acc = function
          | { M2l.name= Some name; expr } ->
              let modname = Modname.v name in
              let prefix = RPath.extend ~modname prefix in
              let acc = RPath.Set.add prefix acc in
              on_module_expr ~prefix acc expr
          | _ -> acc
        in
        List.fold_left fn acc ms
    | _ -> acc
  in
  let ts = List.map (fun { Loc.data; _ } -> data) m2l in
  let prefix = RPath.singleton modname in
  let set = List.fold_left (on_expression ~prefix) RPath.Set.empty ts in
  let fn elt acc = Path.Set.add (RPath.to_path elt) acc in
  RPath.Set.fold fn set Path.Set.empty

let from_source location =
  match Support.extension (Fpath.to_string location) with
  | ("ml" | "mli") as extension -> (
      let current, base = Fpath.split_base location in
      let basename = Fpath.basename base in
      let name = Unitname.modulize (Fpath.to_string location) in
      let kind = if extension = "ml" then M2l.Structure else M2l.Signature in
      let version = None in
      let modname = Unitname.modname name in
      let exports = [ (modname, None) ] in
      match (Uniq_ml.run_into ~current [ basename ], kind) with
      | { Comp_unit.mli= [ u ]; ml= [] }, M2l.Structure ->
          let intfs, impls =
            Deps.all u.Comp_unit.more.Comp_unit.dependencies
            |> List.fold_left to_elt ([], [])
          in
          let format = Format (Ml, u) in
          (* TODO(dinosaure): I don't know what I should add on [modules]. For
             my perspective, it's what the [*.ml] implements and we should look
             into the [u.Comp_unit.code] and see which modules we implements
             ([module Foo = struct ... end]).

             We can probably assert that what is missing for the same [*.cmx]
             must be a superset of what is missing for the given [*.ml].

             On the big perspective, we are mostly interested by what [codept]
             gives to us on source files and then, we should only follow
             [*.cmx{,a}]. *)
          let modules = Path.Set.empty in
          Ok { name; version; modules; exports; intfs; impls; format }
      | { Comp_unit.mli= [ u ]; ml= [] }, M2l.Signature ->
          let intfs, impls =
            Deps.all u.Comp_unit.more.Comp_unit.dependencies
            |> List.fold_left to_elt ([], [])
          in
          let format = Format (Mli, u) in
          let modules = collect_modules_on_mli ~modname u.Comp_unit.code in
          Ok { name; version; modules; exports; intfs; impls; format }
      | { Comp_unit.ml; mli }, _ ->
          Log.err (fun m -> m "ml: @[<hov>%a@]" Fmt.(Dump.list Comp_unit.pp) ml);
          Log.err (fun m ->
              m "mli: @[<hov>%a@]" Fmt.(Dump.list Comp_unit.pp) mli);
          assert false)
  | _ -> error_msgf "Invalid OCaml object: %a" Fpath.pp location

let from_object location { Misc.Magic_number.kind; version } ic =
  let version = Some version in
  match kind with
  | Misc.Magic_number.Cmi -> info_of_cmi ~location ~version ic
  | Cmo -> info_of_cmo ~location ~version ic
  | Cma -> info_of_cma ~location ~version ic
  | Cmx _ -> info_of_cmx ~location ~version ic
  | Cmxa _ -> info_of_cmxa ~location ~version ic
  | _ -> error_msgf "Unexpected OCaml object: %a" Fpath.pp location

let v location =
  OS.File.with_ic location @@ fun ic () ->
  match Misc.Magic_number.read_info ic with
  | Error _ -> from_source location
  | Ok info -> from_object location info ic

let pp ppf t = Fmt.string ppf (Unitname.filepath t.name)

let v location =
  match Result.join (v location ()) with
  | value -> value
  | exception Inconsistency (unit, name, crc, crc') ->
      error_msgf
        "Inconsistency between interfaces:\n\
         The given library %s requires two times the interface %a with\n\
         1) the digest %a\n\
         2) and the digest %a\n"
        (Unitname.filepath unit) Modname.pp name Digest.pp crc Digest.pp crc'

let vs lst =
  let ( let* ) = Result.bind in
  let fn acc path =
    match acc with
    | Error _ as err -> err
    | Ok acc ->
        let* a = v path in
        Ok (a :: acc)
  in
  List.fold_left fn (Ok []) lst

let dummy = String.make (Digest.length * 2) '-'

let show_elt ppf = function
  | Qualified (name, crc) ->
      Fmt.pf ppf "\t%a\t%a\n%!"
        Fmt.(styled `Bold Digest.pp)
        crc
        Fmt.(styled `Yellow Modname.pp)
        name
  | Fully_qualified (name, crc, location) ->
      Fmt.pf ppf "\t%a\t%a (%a)\n%!"
        Fmt.(styled `Bold Digest.pp)
        crc
        Fmt.(styled `Yellow Modname.pp)
        name
        Fmt.(styled `Green Fpath.pp)
        location
  | Named name ->
      Fmt.pf ppf "\t%a\t%a\n%!"
        Fmt.(styled `Bold string)
        dummy
        Fmt.(styled `Yellow Modname.pp)
        name
  | Located (name, location) ->
      Fmt.pf ppf "\t%a\t%a (%a)\n%!"
        Fmt.(styled `Bold string)
        dummy
        Fmt.(styled `Yellow Modname.pp)
        name
        Fmt.(styled `Green Fpath.pp)
        location

let show_export ppf (name, crc) =
  match crc with
  | None ->
      Fmt.pf ppf "\t%a\t%a\n%!"
        Fmt.(styled `Bold string)
        dummy
        Fmt.(styled `Yellow Modname.pp)
        name
  | Some crc ->
      Fmt.pf ppf "\t%a\t%a\n%!"
        Fmt.(styled `Bold Digest.pp)
        crc
        Fmt.(styled `Yellow Modname.pp)
        name

let show_module ppf path = Fmt.pf ppf "\t%a\n%!" Fmt.(styled `Bold Path.pp) path

let show ppf t =
  Fmt.pf ppf "File: %a\n%!"
    Fmt.(styled `Green string)
    (Unitname.filepath t.name);
  Fmt.pf ppf "Name: %a\n%!"
    Fmt.(styled `Yellow Modname.pp)
    (Unitname.modname t.name);
  if Stdlib.Option.is_some t.version then
    Fmt.pf ppf "Version: %d\n%!" (Stdlib.Option.get t.version);
  if t.intfs <> [] then Fmt.pf ppf "Interfaces imported:\n%!";
  List.iter (Fmt.pf ppf "%a" show_elt) t.intfs;
  if t.impls <> [] then Fmt.pf ppf "Implementations imported:\n%!";
  List.iter (Fmt.pf ppf "%a" show_elt) t.impls;
  Fmt.pf ppf "Export:\n%!";
  List.iter (Fmt.pf ppf "%a" show_export) t.exports;
  if Path.Set.is_empty t.modules = false then begin
    Fmt.pf ppf "Modules:\n%!";
    let modules = Path.Set.to_list t.modules in
    List.iter (Fmt.pf ppf "%a" show_module) modules
  end;
  let intfs, impls = missing t in
  let pp_elt ppf = function
    | modname, Some crc ->
        Fmt.pf ppf "\t%a\t%a"
          Fmt.(styled `Bold Digest.pp)
          crc
          Fmt.(styled `Blue Modname.pp)
          modname
    | modname, None ->
        Fmt.pf ppf "\t%a\t%a"
          Fmt.(styled `Bold string)
          dummy
          Fmt.(styled `Red Modname.pp)
          modname
  in
  if intfs <> [] then begin
    Fmt.pf ppf "Missing interfaces:\n%!";
    Fmt.pf ppf "@[<hov>%a@]\n%!" Fmt.(list ~sep:(any "@.") pp_elt) intfs
  end;
  if impls <> [] then begin
    Fmt.pf ppf "Missing implementations:\n%!";
    Fmt.pf ppf "@[<hov>%a@]\n%!" Fmt.(list ~sep:(any ";@.") pp_elt) impls
  end
