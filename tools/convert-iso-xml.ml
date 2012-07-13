let _ =
  let in_file = open_in Sys.argv.(0) in
  try
    List.map (fun line -> print_endline line) 
  with
    | End_of_file ->
        close_in in_file
