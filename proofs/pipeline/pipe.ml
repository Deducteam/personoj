open Feather
open Feather.Infix

let process src proveit qfo_conf =
  (* Define commands *)
  let proveit = process proveit [ "--traces"; "-l"; src ]
  and keepjson = process "perl" [ "-ne"; "print if /^\\{.*\\}$/" ]
  and mkprops =
    process "jq"
      [
        "-r";
        {d|"symbol {|" + .name + "!" + (.incr | tostring) + "|}: " + .dk + ";"|d};
      ]
  and mkdeps =
    process "jq" [ "-r"; {|(.name + "!" + (.incr | tostring)), .path|} ]
  and dopth = process "dopth" []
  and chainprops depfile = process "psnj-chainprops" [ depfile ]
  and foise = process "psnj-qfo" [qfo_conf] in
  let logfile = Filename.remove_extension src ^ ".log" in
  let depfile = Filename.(temp_file (remove_extension src) ".dep") in
  (* Run commands *)
  run (proveit > "/dev/null");
  let json = collect stdout (cat logfile |. keepjson) in
  run (echo json |. mkdeps |. dopth > depfile);
  let propositions =
    collect stdout (echo json |. mkprops |. chainprops depfile |. foise)
  in
  Format.printf "%s" propositions

open Cmdliner

let src =
  let doc = "Rerun proofs of file $(docv) and record log" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"PVS")

let proveit =
  let doc = "Path to the proveit script" in
  Arg.(value & opt string "proveit" & info [ "proveit" ] ~doc)

let qfo_conf =
  let doc = "Configuration for QFO" in
  Arg.(value & opt string "qfo.json" & info [ "qfo" ] ~doc)

let cmd =
  let exits = Term.default_exits in
  let doc = "Pipeline for personoj" in
  ( Term.(const process $ src $ proveit $ qfo_conf),
    Term.(info "psnj-pipe" ~exits ~doc) )

let () = Term.(exit @@ eval cmd)
