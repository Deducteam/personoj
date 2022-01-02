let () =
  let src = "mp.log" in
  let bin = Sys.argv.(1) in
  let out_files = [ "lem1.lp"; "lem2.lp" ] in
  Testing.run_pipe ~src ~out_files bin
