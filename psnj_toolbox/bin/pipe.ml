open Feather
open Feather.Infix

(** Create file containing lambdapi commands that open the encoding. *)
let load_enc_f =
  let fname = Filename.temp_file "psnj_load_enc" ".lp" in
  let oc = open_out fname in
  let ppf = Format.formatter_of_out_channel oc in
  Format.fprintf ppf
    {|require open pipe.encoding.lhol 
  pipe.encoding.pvs_cert
  pipe.encoding.pvs_connectives
  pipe.encoding.propositional_connectives;
|};
  close_out oc;
  fname

(** Create temporary files with rewrite rules used to rewrite
    propositions. *)
let meta_rules_f =
  let fname = Filename.temp_file "psnj_meta_rules" ".lp" in
  let oc = open_out fname in
  let ppf = Format.formatter_of_out_channel oc in
  Format.fprintf ppf
    {|rule true ↪ top;
rule false ↪ bot;
rule ¬ $x ↪ not $x;
rule $x ∧ (λ _, $y[]) ↪ conj $x $y;
rule $x ∨ (λ _, $y[]) ↪ disj $x $y;
rule $x ⇒ (λ _, $y[]) ↪ imp $x $y;
rule @∀ ↪ all;
rule @∃ ↪ ex;
|};
  close_out oc;
  fname

let json (content : Ezjsonm.value list) (oc : out_channel) : unit =
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
    process "psnj"
      [
        "meta";
        "-l";
        load_enc_f;
        "-e";
        "require open pipe.spec;";
        "--meta-load";
        meta_rules_f;
      ]
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
    "require open pipe.encoding.lhol pipe.encoding.propositional_connectives;@\n\
     require pipe.spec;@\n";
  Format.fprintf ppf "%s@." sttprops

let process proveit src =
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
      json (Ezjsonm.get_list Fun.id obj) oc;
      close_out oc)
    dict

open Cmdliner

let src =
  let doc = "Rerun proofs of file $(docv) and record log" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"PVS")

let proveit =
  let doc = "Execute $(docv) to obtain a log file with proof information" in
  Arg.(value & opt (some string) None & info [ "proveit" ] ~doc ~docv:"FILE")

let cmd =
  let exits = Term.default_exits in
  let doc = "Xcheck PVS proofs in LPMT" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) cross checks proofs of PVS specifications. It applies \
         several transformations to the traces of proofs (left by proveit) to \
         finally check each inference step using automatic solvers.";
      `P
        "$(tname) accepts as input either a PVS file, in which case a path to \
         the binary proveit must be provided, or a log file containing the \
         traces of the replay of proofs";
      `P
        "The pipe must be called in a directory where a \"lambdapi.pkg\" file \
         wich declares \"pipe\" as root path lies. The encoding of lambda HOL, \
         PVS-Cert and the connectives must be available as \
         \"pipe.encoding.lhol\", \"pipe.encoding.pvs_cert\", \
         \"pipe.encoding.propositional_connectives\", \
         \"pipe.encoding.pvs_connectives\". All the constants and definitions \
         that are used by the proofs and propositions and that are not part of \
         the encoding must be in the module \"pipe.spec\".";
      `S Manpage.s_examples;
      `P "See the examples in pipe_tests/";
      `S Manpage.s_bugs;
      `P
        "It is required to be able to parse logs because for now, only the \
         Allegro version of PVS can produce such logs. Because Allegro CL is \
         not free, we cannot use it on CIs and such. Accepting produced traces \
         allows to generate statically logs with PVS Allegro and then check \
         these logs on the CI.";
      `P
        "The pipe interleaves calls to external binaries (e.g. perl), to psnj \
         itself and ocaml computations.";
    ]
  in
  (Term.(const process $ proveit $ src), Term.(info "pipe" ~exits ~doc ~man))
