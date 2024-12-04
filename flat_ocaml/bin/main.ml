open Base
open Flat_parser

let () = 
  let input = String.to_list "[1,200,[ 2 ,30" in
  let input2 = String.to_list "00 ]],[1,3]]" in
  let read_count = ref 0 in
  
  let read_more () = 
    if !read_count = 0 then begin
      Int.incr read_count;
      More input2
    end else 
      End
  in

  let handle state value =
    Stdio.printf "Found value: %s\n" (show_parse_result value);
    value :: state
  in

  match value_stream input [] handle read_more with
  | Ok results -> 
      Stdio.printf "Final results:\n";
      List.iter results ~f:(fun r -> 
        Stdio.printf "%s\n" (show_parse_result r))
  | Error err -> 
      Stdio.printf "Error: %s\n" (show_parse_error err)
