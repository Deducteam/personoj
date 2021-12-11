open Common
open Parsing
module S = Syntax

type decl = Deps.Id.t * S.p_term
(** A proposition declaration with a name and a type. *)

(** [pp ppf decl] pretty prints declaration [decl] to formatter [ppf]. *)
let pp (ppf : Format.formatter) ((n, ty) : decl) : unit =
  Format.fprintf ppf "@[symbol %a:@ %a;@]" Deps.Id.pp n Pretty.term ty

(** [propositions ast] returns the list of propositions as pairs
    [(name,ty)] where [name] is the name of the proposition and [ty]
    is its type. *)
let propositions (ast : S.ast) : decl list =
  let props : decl list ref = ref [] in
  let match_decl = function
    | S.P_symbol { p_sym_nam; p_sym_typ = Some ty; _ } ->
        (Deps.Id.make p_sym_nam.elt, ty)
    | _ ->
        failwith
          "Invalid Dedukti source file: only symbol declarations are supported"
  in
  Stream.iter (fun c -> props := match_decl c.Pos.elt :: !props) ast;
  List.rev !props

(** [merge imp deps props] transform each proposition of [props] into
    implications (the implication is defined by [imp]) from its
    dependencies specified in [deps] to itself. *)
let merge (imp : S.p_term) (deps : Deps.t) (props : decl list) : decl list =
  let fn ((name, ty) : decl) : decl =
    let deps = try Deps.get name deps with Not_found -> [] in
    let f dep acc =
      let d_ty = List.assoc dep props in
      Syntax.P.(appl (appl imp d_ty) acc)
    in
    let ty = List.fold_right f deps ty in
    (name, ty)
  in
  List.map fn props
