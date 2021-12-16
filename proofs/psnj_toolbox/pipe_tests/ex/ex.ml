let () =
  let check = false in
  let out_files = [ "exi.lp" ] in
  let src = "ex.log" in
  Testing.run_pipe ~src ~out_files ~check Sys.argv.(1)
