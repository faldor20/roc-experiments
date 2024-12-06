open Base
open Flat_parser.Flat

let read_file_chunks filename chunk_size =
  let channel = In_channel.open_text filename in
  let chunk = Bytes.create chunk_size in
  let read_next _ =
    match In_channel.input channel chunk 0 chunk_size with
    | 0 -> End (* EOF reached *)
    | n ->
      if n <> chunk_size
      then (
        let final = Bytes.sub chunk ~pos:0 ~len:n in
        (* Stdio.printf "read bytes %d:\n  %s \n" n (Bytes.to_string final); *)
        More final)
      else
        (* Stdio.printf "read bytes %d:\n  %s \n" n (Bytes.to_string chunk); *)
        More chunk
  in
  read_next, (fun () -> In_channel.close channel), chunk
;;

module FlatTest = struct
  let run_test () =
    let read_next, cleanup, chunk = read_file_chunks "input.txt" 10000 in
    let handle state value =
      (* Stdio.printf "Found value: %s\n" (show_parse_result value); *)
      (* value :: state *)
      state + 1
    in
    let initial = read_next chunk in
    match value_stream chunk 1 handle read_next with
    | Ok results ->
      cleanup ();
      Stdio.printf "Final results:\n %i " results
      (* List.iter results ~f:(fun r -> *)
      (* Stdio.printf "%s\n" (show_parse_result r)) *)
    | Error err ->
      cleanup ();
      Stdio.printf "Error: %s\n" (show_parse_error err)
  ;;
end

module EffectfulTest = struct
  open Flat_parser.Effectful_parser

  let read_file_chunks filename chunk_size =
    let channel = In_channel.open_text filename in
    let chunk = Bytes.create chunk_size in
    let read_next _ =
      match In_channel.input channel chunk 0 chunk_size with
      | 0 -> Bytes.create 0 (* EOF reached *)
      | n ->
        if n <> chunk_size
        then (
          let final = Bytes.sub chunk ~pos:0 ~len:n in
          final)
        else chunk
    in
    read_next, (fun () -> In_channel.close channel), chunk
  ;;

  let run_test () =
    let read_next, cleanup, chunk = read_file_chunks "input.txt" 10000 in
    let handler state value =
      (* Stdio.printf "Found value: %s\n" (show_parse_result value); *)
      state + 1
    in
    let res = handle_parser (startParse handler 0) read_next in
    cleanup ();
    Stdio.printf "Final results:\n %i " res
  ;;
end

let () = FlatTest.run_test ()
