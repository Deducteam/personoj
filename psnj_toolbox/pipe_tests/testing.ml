let lp src = Filename.remove_extension src ^ ".lp"

(** [run_pipe ~src ?check ~out_files ~bin] runs the whole pipeline to
    produce proofs from file [src] and it displays files [out_files]. If
    [check] is true (the default), files are also type checked. *)
let run_pipe ~src ?(check = true) ?(out_files = [ lp src ]) bin =
  let cmd = Filename.quote_command bin [ "pipe"; src ] in
  if check then Common.Library.set_lib_root None;
  let check_out outf =
    Format.printf "=> %s:@." outf;
    if not (Sys.file_exists outf) then (
      Format.eprintf "File \"%s\" has not been produced" outf;
      exit 1);
    ignore @@ Sys.command @@ Filename.quote_command "cat" [ outf ];
    if check then (
      Common.(
        Timed.(Console.verbose := 0);
        try
          ignore @@ Handle.Compile.Pure.compile_file outf;
          Format.printf "=> checked@."
        with Error.Fatal (p, msg) -> (
          match p with
          | Some p ->
              Format.printf "=> check failed: [%a] %s@." Common.Pos.short p msg
          | None -> Format.printf "=> check failed: %s@." msg)))
  in
  match Sys.command cmd with
  | 0 -> List.iter check_out out_files
  | e ->
      Format.eprintf "Pipe failed with exit code %d@." e;
      exit 1
