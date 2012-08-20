open CamomileLibraryDefault
open Camomile

module UTF8Line = Camomile.ULine.Make(Camomile.UTF8)

type vec =
    {
      fn : string;
      func_seen : (string, int) Hashtbl.t;
      max_tc : int;
    }

let is_empty_word word =
  if word = "" then
    true
  else 
    false

let remove_tag word =
  Pcre.replace ~pat:"/\\w+" ~templ:"" word

let load_file filename =
  let ufh = new UTF8Line.input_line 
    (new CharEncoding.in_channel CharEncoding.utf8 (open_in filename)) in
  let words = ref [] in
  let regex = Pcre.regexp "\\s+|\\.|;" in
  try 
    while true; do
      let line = (ufh#get ()) in
      let elems = (List.filter (fun w -> not (is_empty_word w)) (Pcre.split ~rex:regex line)) in
      words :=  List.append !words elems
    done;
    !words
  with
    | End_of_file ->
        ufh#close_in ();
        !words

let get_func_words words = 
  List.rev_map (fun fw -> Pcre.replace ~pat:"/FUNC" ~templ:"" fw) (List.filter (fun w -> if Pcre.pmatch ~pat:"/FUNC" w then true else false) words)

let create_word_feq words =
  let seen = Hashtbl.create (List.length words) in
  List.iter (fun w ->
    Hashtbl.replace seen w (try (succ (Hashtbl.find seen w)) with Not_found -> 1)
  ) words;
  seen

let calc_max_tc words =
  let rec find_max tuples max =
    match tuples with
      | (_, num)::xs ->
          if num >= max then
            find_max xs num
          else
            find_max xs max
      | [] -> max
  in 
  let no_tag_words = List.rev_map (remove_tag) words in
  let seen = create_word_feq no_tag_words in
  let tuples = Hashtbl.fold (fun k v acc -> (k,v)::acc) seen [] in 
  find_max tuples 0

let create_vec filename =
  let words = load_file filename in
  let func_seen  = create_word_feq (get_func_words words) in
  let max_tc = calc_max_tc words in
  {
    fn = filename;
    func_seen = func_seen;
    max_tc = max_tc;
  }

let string_of_kv k v =
  let str_v = (string_of_int v) in
  (k ^ " " ^ str_v)

let _ = 
  let files = List.rev_map (fun f -> "../texts/bol_book_1/" ^ f) (Array.to_list (Sys.readdir "../texts/bol_book_1/")) in
  let vecs = List.rev_map create_vec files in
  let vec = (List.nth vecs 0) in
  print_endline vec.fn;
  Hashtbl.iter (fun k v -> (print_endline (string_of_kv k v))) vec.func_seen
(*  let o_fh = open_out "text_matrix.dat" in
  let row_fh = open_out "rows.dat" in
*)
















