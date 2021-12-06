open Feather
open Feather.Infix

let process src proveit qfo_conf encoding specification =
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
  and dopth = process "psnj-dopth" []
  and chainprops depfile = process "psnj-chainprops" [ depfile ]
  and foise =
    process "psnj-qfo"
      [
        qfo_conf;
        "--map-dir";
        "lpvs:" ^ encoding;
        "--map-dir";
        "spec:" ^ specification;
        "-e";
        "require open spec.main;";
      ]
  and appaxiom = process "psnj-appaxiom" [ "-a"; "Prf" ] in
  (* Set some file names *)
  let logfile =
    (* File produced by proveit *)
    Filename.remove_extension src ^ ".log"
  in
  let depfile =
    Filename.(temp_file (remove_extension src |> basename) ".dep")
  in
  (* Run commands *)
  debug := true;
  run (proveit > "/dev/null");
  let json = collect stdout (cat logfile |. keepjson) in
  run (echo json |. mkdeps |. dopth > depfile);
  let sttprops =
    collect stdout
      (echo json |. mkprops |. foise |. chainprops depfile |. appaxiom)
  in
  Format.printf "%s" sttprops

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

let encoding =
  let doc =
    "Use encoding $(docv) to translate files from full blown PVS-Cert to STT."
  in
  Arg.(required & pos 1 (some dir) None & info [] ~doc ~docv:"ENC")

let specification =
  let doc = "Open module $(docv).main to translate propositions" in
  Arg.(required & pos 2 (some dir) None & info [] ~doc ~docv:"MOD")

let cmd =
  let exits = Term.default_exits in
  let doc = "Pipeline for personoj" in
  ( Term.(const process $ src $ proveit $ qfo_conf $ encoding $ specification),
    Term.(info "psnj-pipe" ~exits ~doc) )

let () = Term.(exit @@ eval cmd)
