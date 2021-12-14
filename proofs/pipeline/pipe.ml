open Feather
open Feather.Infix

let json (qfo_conf : string) (content : Ezjsonm.value list) (oc : out_channel) :
    unit =
  let ppf = Format.formatter_of_out_channel oc in
  let depfile = Filename.temp_file "psnj_pipe" ".dep" in
  let mkprop (obj : Ezjsonm.value) : string =
    let name = Ezjsonm.find obj [ "name" ] |> Ezjsonm.get_string in
    let incr = Ezjsonm.find obj [ "incr" ] |> Ezjsonm.get_int in
    let dk = Ezjsonm.find obj [ "dk" ] |> Ezjsonm.get_string in
    Format.sprintf "@[symbol@ {|%s!%d|}:@ %s;@]" name incr dk
  in
  let dopth = process "psnj" [ "dopth" ] in
  let foise =
    process "psnj" [ "qfo"; qfo_conf; "-e"; "require open qfo.spec.main;" ]
  in
  let declaration_name (obj : Ezjsonm.value) : string =
    let open Ezjsonm in
    let name = find obj [ "name" ] |> get_string in
    let incr = find obj [ "incr" ] |> get_int in
    let path = find obj [ "path" ] |> get_string in
    Format.sprintf "%s!%d@\n%s" name incr path
  in
  let chainprops depfile = process "psnj" [ "chainprops"; depfile ] in
  let appaxiom = process "psnj" [ "appaxiom"; "-a"; "Prf" ] in
  let solve = process "psnj" [ "autosolve"; "--fixed" ] in
  let decl_names = List.map declaration_name content |> String.concat "\n" in
  run (echo decl_names |. dopth > depfile);
  let sttprops =
    let props = List.map mkprop content in
    let props = String.concat "\n" props in
    collect stdout
      (echo props |. foise |. chainprops depfile |. appaxiom |. solve)
  in
  Format.fprintf ppf
    "require open qfo.encoding.lhol qfo.encoding.propositional_connectives;@\n";
  Format.fprintf ppf "%s@." sttprops

let process proveit src qfo_conf =
  (* Define commands *)
  let proveit =
    Option.map (fun p -> process p [ "--traces"; "-l"; src ]) proveit
  in
  let keepjson = process "perl" [ "-ne"; "print if /^\\{.*\\}$/" ] in
  let group = process "psnj" [ "jgroup" ] in
  (* Set some file names *)
  let logfile =
    (* File produced by proveit if proveit is provided, src otherwise *)
    match proveit with
    | Some _ -> Filename.remove_extension src ^ ".log"
    | None -> src
  in
  (* Run commands *)
  Option.iter (fun proveit -> run (proveit > Filename.null)) proveit;
  (* Create one json file per lemma *)
  let dict : (string * Ezjsonm.value) list =
    let out = collect stdout (cat logfile |. keepjson |. group) in
    Ezjsonm.(get_dict (from_string out))
  in
  List.iter
    (fun (n, obj) ->
      let outfile = n ^ ".lp" in
      let oc = open_out outfile in
      json qfo_conf (Ezjsonm.get_list Fun.id obj) oc;
      close_out oc)
    dict

open Cmdliner

let src =
  let doc = "Rerun proofs of file $(docv) and record log" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"PVS")

let qfo_conf =
  let doc = "Configuration for QFO" in
  Arg.(value & opt string "qfo.json" & info [ "qfo" ] ~doc)

let proveit =
  let doc = "Execute $(docv) to obtain a log file with proof information" in
  Arg.(value & opt (some string) None & info [ "proveit" ] ~doc ~docv:"FILE")

let cmd =
  let exits = Term.default_exits in
  let doc = "Pipeline for personoj" in
  let man = [] in
  ( Term.(const process $ proveit $ src $ qfo_conf),
    Term.(info "psnj-pipe" ~exits ~doc ~man) )

let () = Term.(exit @@ eval cmd)
