open CamomileLibraryDefault
open Camomile

module UTF8Line = Camomile.ULine.Make(Camomile.UTF8)

type tale = 
{
  title : string;
  body : string list
}

let create_xml_doc fn =
  let source = Pxp_types.from_file fn in
    Pxp_tree_parser.parse_wfdocument_entity 
      Pxp_types.default_config
      source
      Pxp_tree_parser.default_spec
  
let remove_newline n =
  Pcre.replace ~pat:"\n" ~templ:" " n#data

let create_tale n =
  match n#sub_nodes with
    | x::xs ->
        { title = x#data; body = (List.rev_map remove_newline xs) }
    | [] -> { title = "no title"; body = [""] }

let process_doc doc =
  let root = doc#root in
  List.rev_map create_tale root#sub_nodes

let output_tales dir t =
  let path =("./" ^ dir ^ "/" ^ t.title ^ ".txt") in
  let ufh_out = new UTF8Line.output_line 
    (new CharEncoding.out_channel CharEncoding.utf8 (open_out path)) in
  List.iter (fun p -> ufh_out#put (p ^"\n")) t.body;
  ufh_out#flush ();
  ufh_out#close_out ()
  
let _ =
  try
    let doc = create_xml_doc Sys.argv.(1) in
    let tales = process_doc doc in
    List.iter (output_tales "texts") tales
  with
    | Pxp_types.Validation_error _ 
    | Pxp_types.WF_error _
    | Pxp_types.Namespace_error _
    | Pxp_types.Error _
    | Pxp_types.At(_,_) as error ->
        print_endline ("Pxp error: " ^ Pxp_types.string_of_exn error)







