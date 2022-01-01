module StrMap = Map.Make (String)

let add (k : string) (o : Ezjsonm.value) (m : Ezjsonm.value list StrMap.t) :
    Ezjsonm.value list StrMap.t =
  let objs =
    match StrMap.find_opt k m with None -> [ o ] | Some objs -> o :: objs
  in
  StrMap.add k objs m

let group minify group_by =
  let groups = ref StrMap.empty in
  (try
     while true do
       let line = input_line stdin in
       let json : Ezjsonm.value = Ezjsonm.from_string line in
       let name = Ezjsonm.(find json [ group_by ] |> get_string) in
       groups := add name json !groups
     done
   with End_of_file -> ());
  let m =
    StrMap.map List.rev !groups
    |> StrMap.map (Ezjsonm.list Fun.id)
    |> StrMap.map Ezjsonm.value
  in
  let obj = StrMap.to_seq m |> List.of_seq |> Ezjsonm.dict in
  Ezjsonm.to_channel ~minify stdout obj

open Cmdliner

let minify = Arg.(value & flag & info [ "m" ])

let group_by =
  let doc = "Group objects using field $(docv)" in
  Arg.(value & opt string "name" & info [ "group-by"; "g" ] ~doc ~docv:"FIELD")

let cmd =
  let doc = "Group JSON objects in arrays depending on a field" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) is a filter that reads a list of JSON objects and group them \
         in a dictionary. Groups are determined by the value of a name (which \
         is-confusingly-$(b,name) by default). The values associated to this \
         name become the keys of the dictionary.";
      `S Manpage.s_examples;
      `P "Given JSON objects";
      `Pre
        {|{ "name": "foo", "attr": "baz"   }
{ "name": "bar", "attr": "frobz" }
{ "name": "foo", "attr": "nitz"  }|};
      `P "The output of $(tname) -g name is";
      `Pre
        {|{
    "bar": [
      {
        "name": "bar",
        "attr": "frobz"
      }
    ],
    "foo": [
      {
        "name": "foo",
        "attr": "nitz"
      },
      {
        "name": "foo",
        "attr": "baz"
      }
    ]
  }|};
    ]
  in
  (Term.(const group $ minify $ group_by), Term.info "jgroup" ~doc ~man)
