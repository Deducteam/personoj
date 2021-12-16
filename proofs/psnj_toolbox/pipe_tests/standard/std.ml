let () =
  let out_files =
    [ "simple_imp.lp"; "modus_ponens.lp"; "fa_hyp.lp"; "fa_instantiate.lp" ]
  in
  let check = false in
  (* It fails on fa_hyp_use because lambdapi doesn't handle yet quantifiers *)
  Testing.run_pipe ~check ~out_files ~src:"std.log" Sys.argv.(1)
