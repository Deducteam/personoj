open Cmdliner

let default_cmd =
  let doc = "A set of tools to manipulate propositions as inference steps" in
  let exits = Term.default_exits in
  (Term.(ret @@ const @@ `Help (`Pager, None)), Term.info "psnj" ~doc ~exits)

let cmds =
  [
    Dopth.cmd;
    Chainprops.cmd;
    Appaxiom.cmd;
    Autosolve.cmd;
    Qfo.cmd;
    Split.cmd;
    Group.cmd;
  ]

let () = Term.(exit @@ eval_choice default_cmd cmds)
