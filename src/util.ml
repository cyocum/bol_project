open CamomileLibraryDefault
open Camomile

module CaseMap = Camomile.CaseMap.Make(Camomile.UTF8)
module UTF8Line = Camomile.ULine.Make(Camomile.UTF8)

let str_compare a b =
  if (Camomile.UTF8.compare a b) = 0 then
    true
  else
    false

let rec list_findi (p : 'a -> bool) lst =
  let rec aux lst pos =
    match lst with
      | x::xs -> 
          if (p x) then 
            pos
          else
            aux xs (succ pos)
      | [] -> raise Not_found
  in 
  aux lst 0

let remove_dups accum str =
  if List.exists (str_compare str) accum then
    accum
  else
    str::accum

let output_func_words func_word_lst =
  let out_ufh = new UTF8Line.output_line 
    (new CharEncoding.out_channel CharEncoding.utf8 (open_out "func_words.txt")) in
  List.iter (fun w -> out_ufh#put w) func_word_lst;
  out_ufh#close_out ()

let range i j = 
  let rec aux n acc =
    if n < i then acc else aux (pred n) (n :: acc)
  in 
  aux j []

  
