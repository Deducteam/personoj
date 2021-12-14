let () =
  match
    Unix.system
      (Filename.quote_command "psnj-pipe"
         [ "--qfo"; "encoding/qfo.json"; "ex.log" ])
  with
  | Unix.WEXITED 0 -> 
      ignore @@ Unix.system (Filename.quote_command "cat" ["exi.lp"])
  | Unix.WEXITED n ->
      Format.eprintf "Command exited with code %d@." n;
      exit 1
  | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
      Format.eprintf "Command stopped by signal";
      exit 2
