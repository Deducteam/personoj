let () =
  let cmd =
    Filename.quote_command "psnj-pipe"
      [ "--qfo"; "encoding/qfo.json"; "mp.log" ]
  in
  match Unix.system cmd with
  | WEXITED 0 ->
      Format.printf "./lem1.lp:@.";
      ignore @@ Unix.system @@ Filename.quote_command "cat" [ "lem1.lp" ];
      Format.printf "@\n./lem2.lp:@.";
      ignore @@ Unix.system @@ Filename.quote_command "cat" [ "lem2.lp" ]
  | _ -> exit 1
