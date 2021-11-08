(** Translate lambdapi terms to PVS-Cert terms, with logical connectors. *)

open Common
open Core
open Parsing
module S = Syntax
module B = Bindlib
module T = Term

(** PVS-Cert terms *)
type term_aux =
  | Var of term B.var
  | Symbol of Term.qident
  | Lambda of term * (term, term) B.binder
  | Psub of term * (term, term) B.binder
  | Pi of term * (term, term) B.binder
  | Appl of term * term
(* TODO: pairs and projections *)

and term = term_aux Pos.loc

let _Var = B.box_var
let _Symbol qid = B.box (Pos.none (Symbol qid))
let _Lambda = B.box_apply2 (fun a b -> Pos.(make a.pos (Lambda (a, b))))
let _Psub = B.box_apply2 (fun a b -> Pos.(make a.pos (Psub (a, b))))
let _Pi = B.box_apply2 (fun a b -> Pos.(make a.pos (Pi (a, b))))
let _Appl = B.box_apply2 (fun a b -> Pos.(make (cat a.pos b.pos) (Appl (a, b))))

let rec lift (t : term) : term B.box =
  let lift_bder cons a b = cons (lift a) (B.box_binder lift b) in
  match t.elt with
  | Var x -> _Var x
  | Symbol qid -> _Symbol qid
  | Lambda (a, b) -> lift_bder _Lambda a b
  | Psub (a, b) -> lift_bder _Psub a b
  | Pi (a, b) -> lift_bder _Pi a b
  | Appl (a, b) -> _Appl (lift a) (lift b)

let mkfree (x : term B.var) : term = Pos.none (Var x)
let pp_qid ppf (_pth, sym) = Format.fprintf ppf "%s" sym
let pp_var ppf v = Format.fprintf ppf "%s" (B.name_of v)

let rec pp_aux wrap (ppf : Format.formatter) (t : term_aux) : unit =
  let open Format in
  let wrap fmt = if wrap then "(" ^^ fmt ^^ ")" else fmt in
  match t with
  | Var x -> fprintf ppf "%a" pp_var x
  | Symbol qid -> pp_qid ppf qid
  | Lambda (a, b) ->
      let x, b = B.unbind b in
      fprintf ppf (wrap "@[\\%a:@ %a.@ %a@]") pp_var x (pp false) a (pp false) b
  | Psub (a, b) ->
      let x, b = B.unbind b in
      fprintf ppf "@[{%a:@ %a@ |@ %a}@]" pp_var x (pp false) a (pp false) b
  | Pi (a, b) ->
      let x, b = B.unbind b in
      fprintf ppf (wrap "@[%a:@ %a@ ->@ %a@]") pp_var x (pp true) a (pp false) b
  | Appl (t, u) -> fprintf ppf (wrap "@[%a@ %a@]") (pp false) t (pp true) u

and pp wrap ppf t = pp_aux wrap ppf t.elt

let pp = pp false

module VMap = Map.Make (struct
  type t = T.tvar

  let compare = B.compare_vars
end)

module type PCERTENC = sig
  val el : T.sym val prf : T.sym
end

let make (sig_st : Sig_state.t) : (module PCERTENC) =
  let find symp =
    try Sig_state.find_sym ~prt:true ~prv:true sig_st (Pos.none symp)
    with Not_found -> invalid_arg "make: symbol not found"
  in
  (module struct let el = find ([], "El") let prf = find ([], "Prf") end)

(** Operate on typechecked terms *)
module Make (Pc : PCERTENC) = struct
  type vmap = term B.var VMap.t

  let match_El (f : T.term -> 'a) (t : T.term) : 'a option =
    match t with Appl (Symb s, u) when s == Pc.el -> Some (f u) | _ -> None

  let match_Prf (f : T.term -> 'a) (t : T.term) : 'a option =
    match t with Appl (Symb s, u) when s == Pc.prf -> Some (f u) | _ -> None

  let rec app_first (x : 'a) (funs : ('a -> 'b option) list) : 'b option =
    match funs with
    | [] -> None
    | f :: fs -> ( match f x with Some y -> Some y | None -> app_first x fs)

  let unbind (b : T.tbinder) (vm : vmap) : term B.var * T.term * vmap =
    let x, b = B.unbind b in
    let x' = B.new_var mkfree (B.name_of x) in
    (x', b, VMap.add x x' vm)

  let rec import (vm : vmap) (t : T.term) : term =
    match app_first t [ match_El (import vm); match_Prf (import vm) ] with
    | Some x -> x
    | None -> (
        match t with
        | TRef _ | Wild | Patt _ | TEnv _ | Plac _ -> assert false
        | Type | Kind -> Error.fatal_no_pos "Type or Kind cannot be translated."
        | Abst (a, b) ->
            let a = import vm a in
            let x, b, vm = unbind b vm in
            let b = import vm b in
            let b = B.bind_var x (lift b) in
            Pos.none (Lambda (a, B.unbox b))
        | Appl (t, u) -> Pos.none (Appl (import vm t, import vm u))
        | Prod (a, b) ->
            let a = import vm a in
            let x, b, vm = unbind b vm in
            let b = import vm b in
            let b = B.bind_var x (lift b) in
            Pos.none (Pi (a, B.unbox b))
        | LLet (_, u, b) -> import vm (B.subst b u)
        | Meta _ -> assert false
        | Symb s -> Pos.none (Symbol (s.T.sym_path, s.T.sym_name))
        | Vari x -> (
            try Pos.none (Var (VMap.find x vm)) with Not_found -> assert false))

  let import : T.term -> term = import VMap.empty
end
