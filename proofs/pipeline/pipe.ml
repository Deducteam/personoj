open Feather
open Feather.Infix

let process proveit src qfo_conf encoding specification =
  (* Define commands *)
  let proveit =
    Option.map (fun p -> process p [ "--traces"; "-l"; src ]) proveit
  and keepjson = process "perl" [ "-ne"; "print if /^\\{.*\\}$/" ]
  and mkprops =
    process "jq"
      [
        "-r";
        {d|"symbol {|" + .name + "!" + (.incr | tostring) + "|}: " + .dk + ";"|d};
      ]
  and mkdeps =
    process "jq" [ "-r"; {|(.name + "!" + (.incr | tostring)), .path|} ]
  and dopth = process "psnj-dopth" []
  and foise =
    process "psnj-qfo"
      [
        qfo_conf;
        "--map-dir";
        "qfo:" ^ encoding;
        "--map-dir";
        "spec:" ^ specification;
        "-e";
        "require open spec.main;";
      ]
  and chainprops depfile = process "psnj-chainprops" [ depfile ]
  and appaxiom = process "psnj-appaxiom" [ "-a"; "Prf" ] 
  and solve = process "psnj-autosolve" [ "--fixed" ] in
  (* Set some file names *)
  let logfile =
    (* File produced by proveit if proveit is provided, src otherwise *)
    match proveit with
    | Some _ -> Filename.remove_extension src ^ ".log"
    | None -> src
  in
  let depfile =
    Filename.(temp_file (remove_extension src |> basename) ".dep")
  in
  (* Run commands *)
  Option.iter (fun proveit -> run (proveit > "/dev/null")) proveit;
  let json = collect stdout (cat logfile |. keepjson) in
  run (echo json |. mkdeps |. dopth > depfile);
  let sttprops =
    collect stdout
      (echo json |. mkprops |. foise |. chainprops depfile |. appaxiom |. solve)
  in
  Format.printf "%s" sttprops

open Cmdliner

let src =
  let doc = "Rerun proofs of file $(docv) and record log" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"PVS")

let qfo_conf =
  let doc = "Configuration for QFO" in
  Arg.(value & opt string "qfo.json" & info [ "qfo" ] ~doc)

let encoding =
  let doc =
    "Use encoding $(docv) to translate files from full blown PVS-Cert to STT."
  in
  Arg.(required & pos 1 (some dir) None & info [] ~doc ~docv:"ENC")

let specification =
  let doc = "Open module $(docv).main to translate propositions" in
  Arg.(required & pos 2 (some dir) None & info [] ~doc ~docv:"MOD")

let proveit =
  let doc = "Execute $(docv) to obtain a log file with proof information" in
  Arg.(value & opt (some string) None & info [ "proveit" ] ~doc ~docv:"FILE")

let cmd =
  let exits = Term.default_exits in
  let doc = "Pipeline for personoj" in
  let man = [] in
  ( Term.(const process $ proveit $ src $ qfo_conf $ encoding $ specification),
    Term.(info "psnj-pipe" ~exits ~doc ~man) )

let () = Term.(exit @@ eval cmd)
