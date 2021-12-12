open Cmdliner

let default_cmd = (Term.(ret @@ const @@ `Help (`Pager, None)), Term.info "psnj")
let cmds = [ Dopth.cmd; Chainprops.cmd; Appaxiom.cmd; Autosolve.cmd; Qfo.cmd ]
let () = Term.(exit @@ eval_choice default_cmd cmds)
