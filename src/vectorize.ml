open CamomileLibraryDefault
open Camomile

module UTF8Line = Camomile.ULine.Make(Camomile.UTF8)

type term =
    {
      term : Camomile.UTF8.t;
      tf : float;
      pos : int;
    }

type doc =
    {
      fn : string;
      func_seen : (Camomile.UTF8.t, int) Hashtbl.t;
      max_tc : int;
      terms : term list;
    }


let rec list_findi (p : 'a -> int) lst =
  let rec aux lst pos =
    match lst with
      | x::xs -> 
          prerr_string (x ^ " ");
          if (p x) = 0 then 
            pos
          else
            aux xs (succ pos)
      | [] -> prerr_string "WTF 1\n";
          raise Not_found
  in 
  aux lst 0

let str_compare a b =
  if (Camomile.UTF8.compare a b) = 0 then
    true
  else
    false

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

let rec create_func_word_lst docs accum =
  match docs with
    | x::xs ->
        let words = Hashtbl.fold (fun k v acc -> k::acc) x.func_seen [] in
        List.iter (fun w -> print_endline w) words;
        if words = [] then begin prerr_string (x.fn ^ "\n"); prerr_string "WTF 2\n" end;
        let filtered = List.filter (fun w -> if List.exists (str_compare w) accum then false else true) words in 
        create_func_word_lst xs (List.append filtered accum)
    | [] -> accum

let create_doc filename =
  let words = load_file filename in
  let func_seen  = create_word_feq (get_func_words words) in
  let max_tc = calc_max_tc words in
  {
    fn = filename;
    func_seen = func_seen;
    max_tc = max_tc;
    terms = []
  }

let calc_tf doc func_word_lst =
  let calc_term = function
    | (k, v) ->
        begin
          print_endline doc.fn;
          { term = k; 
            tf = (float v) /. (float doc.max_tc); 
            pos = list_findi (Camomile.UTF8.compare k) func_word_lst;
          }
        end
  in 
  let tuples = Hashtbl.fold (fun k v accum -> (k, v)::accum) doc.func_seen [] in
  let terms = List.rev_map calc_term tuples in
  { doc with terms = terms }

let string_of_term term = 
  (string_of_int term.pos) ^ " " ^ (string_of_float term.tf) 

let _ = 
  let files = List.rev_map (fun f -> "../texts/bol_book_1/" ^ f) (Array.to_list (Sys.readdir "../texts/bol_book_1/")) in
  let docs = List.rev_map create_doc files in
  let func_word_lst = create_func_word_lst docs [] in
  let docs_terms = List.rev_map (fun doc -> calc_tf doc func_word_lst) docs in
  let out_fh = open_out "text_matrix.dat" in
  List.iter (fun doc -> 
    List.iter (fun term ->
      output_string out_fh ((string_of_term term) ^ " ")
    ) doc.terms;
    output_string out_fh "\n"
  ) docs_terms
