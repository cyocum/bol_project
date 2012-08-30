open CamomileLibraryDefault
open Camomile

module UTF8Line = Camomile.ULine.Make(Camomile.UTF8)
module CaseMap = Camomile.CaseMap.Make(Camomile.UTF8)
module UTF8Hash = Hashtbl.Make(
  struct
    type t = Camomile.UTF8.t
    let equal a b =
      if (Camomile.UTF8.compare a b) = 0 then
        true
      else
        false
    let hash = Hashtbl.hash
  end
)

type term =
    {
      term : Camomile.UTF8.t;
      tf : float;
      pos : int;
      idf : float;
    }

type doc =
    {
      fn : string;
      func_seen : int UTF8Hash.t;
      max_tc : int;
      terms : term list;
    }

let is_empty_word word =
  if word = "" then
    true
  else 
    false

let remove_tag word =
  Pcre.replace ~pat:"/\\w+[\\.;,]?" ~templ:"" word 

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
  List.rev_map remove_tag (List.filter (fun w -> if Pcre.pmatch ~pat:"/FUNC" w then true else false) words)

let create_word_count words =
  let seen = UTF8Hash.create (List.length words) in
  List.iter (fun w ->
    UTF8Hash.replace seen w (try (succ (UTF8Hash.find seen w)) with Not_found -> 1)
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
  let seen = create_word_count no_tag_words in
  let tuples = UTF8Hash.fold (fun k v acc -> (k,v)::acc) seen [] in 
  find_max tuples 0

let rec create_func_word_lst docs accum =
  match docs with
    | x::xs ->
        let words = UTF8Hash.fold (fun k v acc -> (CaseMap.lowercase k)::acc) x.func_seen [] in
        create_func_word_lst xs (List.append words accum)
    | [] -> List.fold_left Util.remove_dups [] accum

let calc_idf term docs =
  let find_seen_terms doc =
    if UTF8Hash.mem doc.func_seen term.term then
      1
    else
      0
  in 
  let num_docs_seen = List.fold_left (+) 0 (List.rev_map find_seen_terms docs) in 
  let num_docs = List.length docs in
  let quotient = (float (abs num_docs)) /. (float (succ (abs num_docs_seen))) in 
  { term with idf = (log quotient) }

let create_doc filename =
  let words = load_file filename in
  let func_seen  = create_word_count (get_func_words words) in
  let max_tc = calc_max_tc words in
  {
    fn = filename;
    func_seen = func_seen;
    max_tc = max_tc;
    terms = []
  }

let calc_tf func_word_lst doc  =
  let calc_term = function
    | (k, v) ->
        begin
          { term = k; 
            tf = (float v) /. (float doc.max_tc); 
            (* the position in the matrix that is out put starts at 1 *not* 0 *)
            pos =  (succ (Util.list_findi (Camomile.UTF8.compare (CaseMap.lowercase k)) func_word_lst));
            idf = 0.0;
          }
        end
  in 
  let tuples = UTF8Hash.fold (fun k v accum -> (k, v)::accum) doc.func_seen [] in
  let terms = List.rev_map calc_term tuples in
  { doc with terms = terms }

let string_of_term term = 
  (string_of_int term.pos) ^ " " ^ (string_of_float (term.tf *. term.idf)) 

let list_of_dirs =
[
  "../texts/bol_book_1/";
  "../texts/bol_book_2/";
  "../texts/bol_book_3/";
  "../texts/bol_book_4/"
]

let create_full_path_files dir =
  let files = Array.to_list (Sys.readdir dir) in
  List.rev_map (fun f -> (dir ^ f)) files

let get_files () =
  let filenames = List.rev_map create_full_path_files list_of_dirs in 
  (List.flatten filenames)

let _ = 
  let files = get_files () in
  let docs = List.rev_map create_doc files in
  let func_word_lst = create_func_word_lst docs [] in
  let docs_terms = List.rev_map (calc_tf func_word_lst) docs in
  let output_mat_fh = open_out "text.mat" in
  let output_mat_rows = open_out "rows.mat" in
  let non_zero_terms = List.fold_left (+) 0 (List.rev_map (fun doc -> (List.length doc.terms)) docs_terms) in
  Util.output_func_words func_word_lst;
  output_string output_mat_fh ((string_of_int (List.length docs_terms) ^ " " ^ (string_of_int (List.length func_word_lst)) ^ " " ^ (string_of_int non_zero_terms)) ^ "\n");
  List.iter     
    (fun doc ->
      output_string output_mat_rows (doc.fn ^ "\n");
      List.iter (fun term ->
        output_string output_mat_fh ((string_of_term (calc_idf term docs_terms)) ^ " ")
      ) doc.terms;
      output_string output_mat_fh "\n"
    ) docs_terms;
  close_out output_mat_fh;
  close_out output_mat_rows
