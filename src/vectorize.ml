open CamomileLibraryDefault
open Camomile

module UTF8Line = Camomile.ULine.Make(Camomile.UTF8)

type vec =
    {
      avg_word_len : float;
      num_func_words : int;
      num_cop : int;
      num_src : int;
      num_pp : int;
      num_fv : int;
      num_vn : int;
      ratio_fv_vn : float;
    }

let is_empty_word word =
  if word = "" then
    true
  else 
    false

let remove_tag word =
  Pcre.replace ~pat:"/\\w{3}" ~templ:"" word

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

let calc_avg_word_len words =
  let num_words = List.length words in
  let num_chars = (List.fold_left (+) 0 (List.rev_map String.length words)) in
  (float num_chars) /. (float num_words)

let calc_num_tag_words words tag =
  List.length (List.filter (fun w -> if Pcre.pmatch ~pat:tag w then true else false) words)

let create_vec filename =
  let words = load_file filename in
  {
    avg_word_len = calc_avg_word_len (List.rev_map remove_tag words);
    num_func_words = calc_num_tag_words words "/FUN";
    num_cop = calc_num_tag_words words "/COP";
    num_src = calc_num_tag_words words "/SRC";
    num_pp = calc_num_tag_words words "/PP";
    num_fv = calc_num_tag_words words "/FV";
    num_vn = calc_num_tag_words words "/VN";
    ratio_fv_vn = (float (calc_num_tag_words words "/FV")) /. (float (calc_num_tag_words words "/VN"));
  }

let string_of_vec = function
  | { avg_word_len;
      num_func_words;
      num_cop;
      num_src;
      num_pp;
      num_fv;
      num_vn;
      ratio_fv_vn } ->
      (string_of_float avg_word_len) ^ " " 
      ^ (string_of_int num_func_words) ^ " "
      ^ (string_of_int num_cop) ^ " "
      ^ (string_of_int num_src) ^ " "
      ^ (string_of_int num_pp) ^ " "
      ^ (string_of_int num_fv) ^ " "
      ^ (string_of_int num_vn) ^ " "
      ^ (string_of_float ratio_fv_vn)


let _ = 
  let files = List.rev_map (fun f -> "../texts-test/" ^ f) (Array.to_list (Sys.readdir "../texts-test")) in
  let tuples = List.map create_vec files in
  let o_fh = open_out "text_matrix.dat" in
    List.iter (fun tup -> output_string o_fh ((string_of_vec tup) ^ "\n") ) tuples;
    close_out o_fh















