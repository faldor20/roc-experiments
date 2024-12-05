open Base
open Flat_parser

let read_file_chunks filename chunk_size =
  let channel = In_channel.open_text filename in

  let chunk = Bytes.create chunk_size in
  
  let rec read_next _  =
    match In_channel.input channel chunk 0 chunk_size with
    | 0 -> End  (* EOF reached *)
    | n -> 
      if n <> chunk_size then 
      (
      let final=Bytes.sub chunk ~pos:0 ~len:(n) in
      (* Stdio.printf "read bytes %d:\n  %s \n" n (Bytes.to_string final); *)

      
        More ( final)
      )

      else 
      (
      (* Stdio.printf "read bytes %d:\n  %s \n" n (Bytes.to_string chunk); *)
      More chunk
      )
  in
  read_next, (fun () -> In_channel.close channel),chunk

let () = 
  let read_next, cleanup, chunk = read_file_chunks "input.txt" 10000 in

  let handle state value =
   
    (* Stdio.printf "Found value: %s\n" (show_parse_result value); *)
    (* value :: state *)
    state+1
  in
  let initial= read_next chunk in

  match value_stream chunk 1 handle read_next with
  | Ok results -> 
      cleanup ();
      Stdio.printf "Final results:\n %i "results
      (* List.iter results ~f:(fun r ->  *)
        (* Stdio.printf "%s\n" (show_parse_result r)) *)
  | Error err -> 
      cleanup ();
      Stdio.printf "Error: %s\n" (show_parse_error err)

