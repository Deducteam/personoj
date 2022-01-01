open Core
module T = Term
module LibT = LibTerm

let rewrite_with (sign : Sign.t) (rules : (Term.sym * Term.rule) list) :
    Term.term -> Term.term =
  let open Timed in
  let open Lplib.Extra in
  let exec t =
    (* Remove rules from signature *)
    let strip_sym _ (s, _) =
      s.T.sym_rules := [];
      s.T.sym_def := None;
      Tree.update_dtree s
    in
    StrMap.iter strip_sym !(sign.Sign.sign_symbols);
    Common.Path.Map.iter
      (fun _ s -> StrMap.iter strip_sym !(s.Sign.sign_symbols))
      Timed.(!Sign.loaded);
    (* Add a set of custom rules *)
    List.iter (fun (s, r) -> Sign.add_rule sign s r) rules;
    Eval.snf [] t
  in
  pure_apply exec
