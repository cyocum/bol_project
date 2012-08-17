open CamomileLibraryDefault
open Camomile

module UTF8Line = Camomile.ULine.Make(Camomile.UTF8)

type vec =
    {
      fn : string;
      avg_word_len : float;
      avg_sent_len : float;
      num_func_words : int;
      num_cop : int;
      num_src : int;
      num_pp : int;
      num_fv : int;
      num_vn : int;
      num_punc : int;
      num_roman_num : int;
      ratio_fv_vn : float;
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

let calc_avg_word_len words =
  let num_words = List.length words in
  let num_chars = (List.fold_left (+) 0 (List.rev_map String.length words)) in
  (float num_chars) /. (float num_words)

let calc_num_tag_words words tag =
  List.length (List.filter (fun w -> if Pcre.pmatch ~pat:tag w then true else false) words)

let cal_avg_sent_len words num_punc =
  if num_punc = 0 then
    0.0
  else
    let num_words = List.length words in 
      (float num_words) /. (float num_punc)

let cal_rfv num_fv num_vn =
  if num_vn = 0 then
    0.0
  else
    ((float num_fv) /. (float num_vn))

let create_vec filename =
  let words = load_file filename in
  let num_fv = calc_num_tag_words words "/FV" in
  let num_vn = calc_num_tag_words words "/VN" in
  let num_punc = calc_num_tag_words words "/PUNC" in
  {
    fn = filename;
    avg_word_len = calc_avg_word_len (List.rev_map remove_tag words);
    avg_sent_len = cal_avg_sent_len words num_punc;
    num_func_words = calc_num_tag_words words "/FUN";
    num_cop = calc_num_tag_words words "/COP";
    num_src = calc_num_tag_words words "/SRC";
    num_pp = calc_num_tag_words words "/PP";
    num_fv = num_fv;
    num_vn = num_vn;
    num_punc = num_punc;
    num_roman_num = calc_num_tag_words words "/NUM";
    ratio_fv_vn = cal_rfv num_fv num_vn;
  }

let string_of_vec vec = 
  (string_of_float vec.avg_word_len) ^ " " 
  ^ (string_of_float vec.avg_sent_len) ^ " " 
  ^ (string_of_int vec.num_func_words) ^ " "
  ^ (string_of_int vec.num_cop) ^ " "
  ^ (string_of_int vec.num_src) ^ " "
  ^ (string_of_int vec.num_pp) ^ " "
  ^ (string_of_int vec.num_fv) ^ " "
  ^ (string_of_int vec.num_vn) ^ " "
  ^ (string_of_int vec.num_punc) ^ " "
  ^ (string_of_float vec.ratio_fv_vn) ^ " #"
  ^ vec.fn

let _ = 
  let files = List.rev_map (fun f -> "../texts/bol_book_1/" ^ f) (Array.to_list (Sys.readdir "../texts/bol_book_1/")) in
  let vecs = List.rev_map create_vec files in
  let o_fh = open_out "text_matrix.dat" in
  let row_fh = open_out "rows.dat" in
    List.iter (fun vec -> 
      output_string o_fh ((string_of_vec vec) ^ "\n");
      output_string row_fh (vec.fn ^ "\n")
    ) vecs;
    close_out o_fh
