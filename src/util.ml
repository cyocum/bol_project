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

let str_compare a b =
  let regex = Pcre.regexp ~flags:[`CASELESS; `UTF8] a in
  Pcre.pmatch ~rex:regex b
