(** Translate lambdapi terms to PVS-Cert terms, with logical connectors. *)

open Common
open Core
open Parsing
module S = Syntax
module B = Bindlib
module T = Term

(** [app_first x funs] applies the functions in [funs] to [x] until one 
    returns [Some y]. *)
let rec app_first (x : 'a) (funs : ('a -> 'b option) list) : 'b option =
  match funs with
  | [] -> None
  | f :: fs -> ( match f x with Some y -> Some y | None -> app_first x fs)

(** PVS-Cert terms *)
type term =
  | Var of term B.var
  | Symbol of Term.qident
  | Lambda of term * (term, term) B.binder
  | Psub of term * (term, term) B.binder  (** [Psub(a,p)] is [{x: a | p}]. *)
  | Pi of term * (term, term) B.binder
  | Appl of term * term
  | Pair of term * term * term * term
      (** [Pair(a,p,t,h)] is the pair [<t,h>] of type [{x: a | p}]. *)
  | Left of term * term * term
      (** [Left(a,p,t)] is the left projection of term [t]. *)
  | Right of term * term * term
      (** [Right(a,p,t)] is the right projection of term [t]. *)

(** Bindlib stuff. *)

let _Var = B.box_var
let _Symbol qid = B.box (Symbol qid)
let _Lambda = B.box_apply2 (fun a b -> Lambda (a, b))
let _Psub = B.box_apply2 (fun a b -> Psub (a, b))
let _Pi = B.box_apply2 (fun a b -> Pi (a, b))
let _Appl = B.box_apply2 (fun a b -> Appl (a, b))
let _Pair = B.box_apply4 (fun a p x h -> Pair (a, p, x, h))
let _Left = B.box_apply3 (fun a p x -> Left (a, p, x))
let _Right = B.box_apply3 (fun a p x -> Right (a, p, x))

let rec lift (t : term) : term B.box =
  let lift_bder cons a b = cons (lift a) (B.box_binder lift b) in
  match t with
  | Var x -> _Var x
  | Symbol qid -> _Symbol qid
  | Lambda (a, b) -> lift_bder _Lambda a b
  | Psub (a, b) -> lift_bder _Psub a b
  | Pi (a, b) -> lift_bder _Pi a b
  | Appl (a, b) -> _Appl (lift a) (lift b)
  | Pair (a, p, t, h) -> _Pair (lift a) (lift p) (lift t) (lift h)
  | Left (a, p, t) -> _Left (lift a) (lift p) (lift t)
  | Right (a, p, t) -> _Right (lift a) (lift p) (lift t)

let mkfree (x : term B.var) : term = Var x

(** Printing PVS-Cert terms *)

let pp_qid ppf (_pth, sym) = Format.fprintf ppf "%s" sym
let pp_var ppf v = Format.fprintf ppf "%s" (B.name_of v)

let rec pp wrap (ppf : Format.formatter) (t : term) : unit =
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
  | Pair (_, _, t, h) -> fprintf ppf "@[<%a, %a>@]" (pp false) t (pp false) h
  | Left (_, _, t) -> fprintf ppf (wrap "@[left(%a)@]") (pp false) t
  | Right (_, _, t) -> fprintf ppf (wrap "@[right(%a)@]") (pp false) t

let pp = pp false

(** Maps on [T.term] variables. *)
module VMap = Map.Make (struct
  type t = T.tvar

  let compare = B.compare_vars
end)

(** Contain the encoding of PVS-Cert as separate symbols. *)
module type PCERTENC = sig
  val el : T.sym val prf : T.sym
end

(** [make sig_st] creates a PVS-Cert encoding from a signature state, or
    @raise invalid_arg when the signature state does not contain some symbol. *)
let make (sig_st : Sig_state.t) : (module PCERTENC) =
  let find symp =
    try Sig_state.find_sym ~prt:true ~prv:true sig_st (Pos.none symp)
    with Not_found -> invalid_arg "make: symbol not found"
  in
  (module struct let el = find ([], "El") let prf = find ([], "Prf") end)

(** Produce an translator from lpmt terms encoding PVS-Cert to PVS-Cert
    terms when the encoding is given. *)
module Make (Pc : PCERTENC) = struct
  type vmap = term B.var VMap.t

  (** [match_El f t] applies [f] on [u] when [t] is of the form [El u]. It 
      returns [None] otherwise. *)
  let match_El (f : T.term -> 'a) (t : T.term) : 'a option =
    match t with Appl (Symb s, u) when s == Pc.el -> Some (f u) | _ -> None

  (** [match_Prf f t] applies [f] on [u] when [t] is of the form [Prf u]. It 
      returns [None] otherwise. *)
  let match_Prf (f : T.term -> 'a) (t : T.term) : 'a option =
    match t with Appl (Symb s, u) when s == Pc.prf -> Some (f u) | _ -> None

  (** [unbind b vm] is [B.unbind b] but it creates a fresh PVS-Cert
      var and maps the variable produced by [unbind] to this fresh PVS-Cert
      var. *)
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
            Lambda (a, B.unbox b)
        | Appl (t, u) -> Appl (import vm t, import vm u)
        | Prod (a, b) ->
            let a = import vm a in
            let x, b, vm = unbind b vm in
            let b = import vm b in
            let b = B.bind_var x (lift b) in
            Pi (a, B.unbox b)
        | LLet (_, u, b) -> import vm (B.subst b u)
        | Meta _ -> assert false
        | Symb s -> Symbol (s.T.sym_path, s.T.sym_name)
        | Vari x -> ( try Var (VMap.find x vm) with Not_found -> assert false))

  (** [import t] translates a lpmt term [t] to a PVS-Cert term [t]. *)
  let import : T.term -> term = import VMap.empty
end

(** Maps from PVS-Cert vars. *)
module CVMap = Map.Make (struct
  type t = term B.var

  let compare = B.compare_vars
end)

module Tt = Tptp.Term

let unbind (b : (term, term) B.binder) (vm : Tt.t B.var CVMap.t) :
    Tt.t B.var * term * Tt.t B.var CVMap.t =
  let x, b = B.unbind b in
  let x' = B.new_var Tt.mkfree (B.name_of x) in
  (x', b, CVMap.add x x' vm)

exception CannotTranslate of term

(** Mapping lists to build TPTP terms. A mapping [(s,f)] of any of the
    three lists [una_cons], [bin_cons] and [bnd_cons] is used to build
    a TPTP term using [f] applied to the elements the symbol [s] is
    applied to. *)

let una_cons = [ ("¬", fun x -> Tt.Not x) ]

let bin_cons =
  [
    ("⇒", fun x y -> Tt.Imply (x, y))
  ; ("∧", fun x y -> Tt.And (x, y))
  ; ("∨", fun x y -> Tt.Or (x, y))
  ]

let bnd_cons = [ ("∀", fun b -> Tt.All b); ("∃", fun b -> Tt.Ex b) ]

(** [tptp_of t] translate term [t] to a TPTP expression. *)
let rec tptp_of (vm : Tt.t B.var CVMap.t) (t : term) : Tt.t =
  match t with
  (* Specific transformations. *)
  | Appl (Symbol (_, s), t) when List.mem_assoc s una_cons ->
      let cons = List.assoc s una_cons in
      cons (tptp_of vm t)
  (* Transform binary logical connective that are dependent into
     non dependent ones (provided that there is no real dependency). *)
  | Appl (Appl (Symbol (_, s), u), Lambda (_, b)) when List.mem_assoc s bin_cons
    ->
      if not (B.binder_constant b) then raise (CannotTranslate t);
      let _, b = B.unbind b in
      let cons = List.assoc s bin_cons in
      cons (tptp_of vm u) (tptp_of vm b)
  | Appl (Appl (Symbol (_, s), _), Lambda (_, b)) when List.mem_assoc s bnd_cons
    ->
      let cons = List.assoc s bnd_cons in
      let x, b, vm = unbind b vm in
      let b = tptp_of vm b in
      let b = B.bind_var x (Tt.lift b) in
      cons (B.unbox b)
  (* Generic transformations. *)
  | Symbol (_, s) -> Id s
  | Var x -> Tt.Var (CVMap.find x vm)
  | Appl (t, u) -> Tt.App (tptp_of vm t, tptp_of vm u)
  | Lambda (_, b) ->
      let x, b, vm = unbind b vm in
      let b = tptp_of vm b in
      let b = B.bind_var x (Tt.lift b) in
      Lam (B.unbox b)
  | Pair _ | Left _ | Right _ | Psub _ | Pi _ -> raise (CannotTranslate t)

let tptp_of (t : term) : Tt.t = tptp_of CVMap.empty t
