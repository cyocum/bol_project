open CamomileLibraryDefault
open Camomile

module UTF8Line = Camomile.ULine.Make(Camomile.UTF8)
module CaseMap = Camomile.CaseMap.Make(Camomile.UTF8)
module UTF8Hash = Hashtbl.Make(
  struct
    type t = Camomile.UTF8.t
    let equal a b = Util.str_compare a b
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

let remove_tag regex word =
  Pcre.replace ~rex:regex ~templ:"" word

let load_file filename =
  let ufh = new UTF8Line.input_line 
    (new CharEncoding.in_channel CharEncoding.utf8 (open_in filename)) in
  let words = ref [] in
  let regex = Pcre.regexp "\\s+" in
  try 
    while true; do
      let line = (ufh#get ()) in
      let elems = (List.filter (fun w -> not (Util.str_compare w "")) (Pcre.split ~rex:regex line)) in
      words :=  List.rev_append !words elems
    done;
    !words
  with
    | End_of_file ->
        let regex = Pcre.regexp "[\\.;,:\\?\\(\\)!]" in 
        ufh#close_in ();
        List.rev_map (Pcre.replace ~rex:regex ~templ:"") !words

let get_func_words words = 
  let regex = Pcre.regexp "/FUNC" in  
  List.rev_map (remove_tag regex) (List.filter (Pcre.pmatch ~rex:regex) words)

let create_word_count words =
  let seen = UTF8Hash.create (List.length words) in
  List.iter (fun w ->
    let lc_w = (CaseMap.lowercase w) in 
    UTF8Hash.replace seen lc_w (try (succ (UTF8Hash.find seen lc_w)) with Not_found -> 1)
  ) words;
  seen

let calc_max_tc words =
  let rec find_max values max =
    match values with
      | x::xs ->
          if x >= max then
            find_max xs x
          else
            find_max xs max
      | [] -> max
  in 
  let regex = Pcre.regexp "/\\w+" in 
  let no_tag_words = List.rev_map (remove_tag regex) words in
  let seen = create_word_count no_tag_words in
  let values = UTF8Hash.fold (fun _ v acc -> v::acc) seen [] in 
  find_max values 0

let rec create_func_word_lst docs accum =
  match docs with
    | x::xs ->
        let words = UTF8Hash.fold (fun k _ acc -> k::acc) x.func_seen [] in
        create_func_word_lst xs (List.rev_append words accum)
    | [] -> List.fold_left Util.remove_dups [] accum

let calc_idf docs term =
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

let calc_all_idf docs =
  let rec do_calc rem accum =
    match rem with
      | x::xs ->
          let nx = { x with terms = List.rev_map (calc_idf docs) x.terms } in 
          do_calc xs (nx::accum)
      | [] ->
          accum
  in 
  do_calc docs [] 

let create_doc filename =
  let words = load_file filename in
  let func = (get_func_words words) in
  let func_seen  = create_word_count func in
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
            pos =  (Util.list_findi (Util.str_compare k) func_word_lst);
            idf = 0.0;
          }
        end
  in 
  let tuples = UTF8Hash.fold (fun k v accum -> (k, v)::accum) doc.func_seen [] in
  let terms = List.rev_map calc_term tuples in
  { doc with terms = terms }

let string_of_term term = 
  (* shift position by one since cluto uses 1 rather than 0 as its index *)
  (string_of_int (succ term.pos)) ^ " " ^ (string_of_float (term.tf *. term.idf)) 

let create_full_path_files dir =
  let files = Array.to_list (Sys.readdir dir) in
  List.rev_map (fun f -> (dir ^ f)) files

let get_files list_of_dirs =
  let filenames = List.rev_map create_full_path_files list_of_dirs in 
  (List.flatten filenames)

let calc_term terms pos =
  try
    let term = List.find (fun t ->  t.pos = pos) terms in
    term.tf *. term.idf
  with
    | Not_found -> 
        0.0

let term_csv func_word_lst doc =
  (* this has to do with the fact that the length of the
     list is ONE MORE than the number of func words *)
  let r = Util.range 0 (pred (List.length func_word_lst)) in 
  let lst = List.rev_map string_of_float (List.rev_map (calc_term doc.terms) r) in 
  String.concat "," lst

let term_space doc =
  let lst = List.rev_map string_of_term doc.terms in
  String.concat " " lst

let output_results_cluto docs func_word_lst non_zero_terms =
  let output_mat_fh = open_out "text.mat" in
  let output_mat_rows = open_out "rows.mat" in
  output_string output_mat_fh ((string_of_int (List.length docs) ^ " " ^ (string_of_int (List.length func_word_lst)) ^ " " ^ (string_of_int non_zero_terms)) ^ "\n");
  List.iter (fun d -> 
    output_string output_mat_fh ((term_space d) ^ "\n");
    output_string output_mat_rows (d.fn ^ "\n")
  ) docs;
  close_out output_mat_fh;
  close_out output_mat_rows

let output_csv docs func_word_lst =
  let csv_lst = List.rev_map (fun d -> ("\"" ^ d.fn ^ "\"" ^ "," ^ (term_csv func_word_lst d))) docs in 
  let ufh_out = new UTF8Line.output_line 
    (new CharEncoding.out_channel CharEncoding.utf8 (open_out "texts.csv")) in
  print_endline ("func num" ^ (string_of_int (List.length func_word_lst)));
  ufh_out#put (String.concat "," func_word_lst);
  List.iter ufh_out#put csv_lst;
  ufh_out#close_out ()

let list_of_dirs =
  [
    "../texts/bol_book_1/";
    "../texts/bol_book_2/";
    "../texts/bol_book_3/";
    "../texts/bol_book_4/";
    "../texts/bol_book_5/"
  ]

let _ = 
  let files = get_files list_of_dirs in
  let docs = List.rev_map create_doc files in
  let func_word_lst = create_func_word_lst docs [] in
  let docs_terms = List.rev_map (calc_tf func_word_lst) docs in
  let non_zero_terms = List.fold_left (+) 0 (List.rev_map (fun doc -> (List.length doc.terms)) docs_terms) in
  let docs_full = calc_all_idf docs_terms in 
  output_results_cluto (List.rev docs_full) func_word_lst non_zero_terms;
  output_csv docs_full func_word_lst;
  Util.output_func_words func_word_lst
