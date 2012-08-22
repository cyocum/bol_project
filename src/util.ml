open CamomileLibraryDefault
open Camomile

module CaseMap = Camomile.CaseMap.Make(Camomile.UTF8)

let rec list_findi (p : 'a -> int) lst =
  let rec aux lst pos =
    match lst with
      | x::xs -> 
          if (p x) = 0 then 
            pos
          else
            aux xs (succ pos)
      | [] -> raise Not_found
  in 
  aux lst 0

let str_compare a b =
  if (Camomile.UTF8.compare a b) = 0 then
    true
  else
    false
