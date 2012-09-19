open CamomileLibraryDefault
open Camomile

module UTF8Line = Camomile.ULine.Make(Camomile.UTF8)

let load_str filename =
  let labels_fh = new UTF8Line.input_line 
    (new CharEncoding.in_channel CharEncoding.utf8 (open_in filename)) in
  let labels = ref [] in
  let regex = Pcre.regexp " " in 
  try
    while true; do
      let lst = Pcre.split ~rex:regex (labels_fh#get ()) in       
      labels := (List.hd lst)::!labels
    done;
    !labels
  with
    | End_of_file ->
        labels_fh#close_in ();
        !labels

let load_clusters filename =
  let fh = open_in filename in 
  let line = input_line fh in 
  let lst = List.rev_map int_of_string (Pcre.split ~pat:"," line) in 
  close_in fh;
  lst

let tuple_sort a b =
  match a, b with
    | ((h1, clu1), (h2, clu2)) ->
        compare clu1 clu2


let print_clusters tuples =
  List.iter (function (h, c) -> print_endline (h ^ "\t" ^ (string_of_int c))) tuples

let _ =
  let labels = load_str "texts.csv" in
  let cluster_csv_lst = load_clusters "clusters.csv" in 
  let tuples = List.sort tuple_sort (List.combine labels cluster_csv_lst) in 
  print_clusters tuples
