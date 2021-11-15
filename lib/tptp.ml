(* TPTP abstract syntax tree and printer. *)

module Term = struct
  module B = Bindlib

  type t =
    | True
    | False
    | Id of string
    | Var of t B.var
    | App of t * t
    | Not of t
    | And of t * t
    | Or of t * t
    | Imply of t * t
    | Equiv of t * t
    | All of (t, t) B.binder
    | Ex of (t, t) B.binder
    | Lam of (t, t) B.binder

  (** Bindlib stuff for lifting *)

  let _Var = B.box_var
  let _Id s = B.box (Id s)
  let _App = B.box_apply2 (fun t u -> App (t, u))
  let _Not = B.box_apply (fun t -> Not t)
  let _And = B.box_apply2 (fun t u -> And (t, u))
  let _Or = B.box_apply2 (fun t u -> Or (t, u))
  let _Imply = B.box_apply2 (fun t u -> Imply (t, u))
  let _Equiv = B.box_apply2 (fun t u -> Equiv (t, u))
  let _True = B.box True
  let _False = B.box False
  let _All = B.box_apply (fun b -> All b)
  let _Ex = B.box_apply (fun b -> Ex b)
  let _Lam = B.box_apply (fun b -> Lam b)
  let mkfree (x : t B.var) : t = Var x

  let rec lift (t : t) : t B.box =
    let l = lift in
    let lift_bder cons b = cons (B.box_binder l b) in
    match t with
    | True -> _True
    | False -> _False
    | Id s -> _Id s
    | Var x -> _Var x
    | App (t, u) -> _App (l t) (l u)
    | Not t -> _Not (l t)
    | And (t, u) -> _And (l t) (l u)
    | Or (t, u) -> _Or (l t) (l u)
    | Imply (t, u) -> _Imply (l t) (l u)
    | Equiv (t, u) -> _Equiv (l t) (l u)
    | All b -> lift_bder _All b
    | Ex b -> lift_bder _Ex b
    | Lam b -> lift_bder _Lam b

  (** Printing *)

  (** [pp_var ppf x] prints variable [x] to formatter [ppf]. Variables
      are uppercased in TPTP. *)
  let pp_var ppf (x : t B.var) : unit =
    Format.fprintf ppf "%s" (String.capitalize_ascii (B.name_of x))

  (** [pp_id ppf s] prints identifier [s] to formatter [ppf]. Identifiers
      are lowercased in TPTP. *)
  let pp_id ppf (s : string) : unit =
    Format.fprintf ppf "%s" (String.lowercase_ascii s)

  (* TODO handle name clash if [p] and [P] are both different vars (or
     different symbols) in some PVS-Cert term. *)

  let rec pp (wrap : bool) (ppf : Format.formatter) (t : t) : unit =
    let open Format in
    let out fmt = fprintf ppf fmt in
    let wrap fmt = if wrap then "(" ^^ fmt ^^ ")" else fmt in
    match t with
    | True -> out "$true"
    | False -> out "$false"
    | Id s -> pp_id ppf s
    | Var x -> pp_var ppf x
    | App (t, u) -> out (wrap "@[%a@ %a@]") (pp false) t (pp true) u
    | Not t -> out (wrap "@[~@ %a@]") (pp false) t
    | And (t, u) -> out (wrap "@[%a@ & %a@]") (pp true) t (pp true) u
    | Or (t, u) -> out (wrap "@[%a@ | %a@]") (pp true) t (pp true) u
    | Imply (t, u) -> out (wrap "@[%a@ => %a@]") (pp true) t (pp true) u
    | Equiv (t, u) -> out (wrap "@[%a@ <=> %a@]") (pp true) t (pp true) u
    | All u ->
        let x, b = B.unbind u in
        out (wrap "@[!@ [%a]@ :@ %a@]") pp_var x (pp true) b
    | Ex u ->
        let x, b = B.unbind u in
        out (wrap "@[? [%a]@ :@ %a@]") pp_var x (pp true) b
    | Lam u ->
        let x, b = B.unbind u in
        out (wrap "@[`@ [%a]@ :@ %a@]") pp_var x (pp true) b

  (** [pp ppf t] prints PVS-Cert term [t] in TPTP format to formatter [ppf]. *)
  let pp (ppf : Format.formatter) (t : t) : unit = pp false ppf t
end
