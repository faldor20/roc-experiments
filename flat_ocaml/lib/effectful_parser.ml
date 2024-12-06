open Base

(* Effect definitions *)
type _ Stdlib.Effect.t +=
  | Read : bytes Stdlib.Effect.t
  | End_of_input : unit Stdlib.Effect.t

(* Types *)
type token =
  | Open
  | Close
  | Sep
  | Num of int
[@@deriving show]

type parse_result =
  | ListT of parse_result list
  | NumT of int
[@@deriving show]

(* Define exceptions *)
exception TokenizeErr of string
exception InvalidStart of string
exception InvalidNextToken of string
exception MissingAfterVal
exception InvalidAfterParse of char

(* Helper functions *)
let is_whitespace = function
  | ' ' | '\n' | '\r' | '\t' -> true
  | _ -> false
;;

let is_num c = Char.(c >= '0' && c <= '9')

let num_from_bytes numBytes =
  numBytes
  |> List.fold ~init:(0, 1) ~f:(fun (acc, multiplier) c ->
    let digit = Char.to_int c - 0x30 in
    acc + (digit * multiplier), multiplier * 10)
  |> fst
;;

(* Effectful tokenizer *)
let rec eat_whitespace bytes pos =
  if pos >= Bytes.length bytes
  then pos
  else if is_whitespace (Bytes.get bytes pos)
  then eat_whitespace bytes (pos + 1)
  else pos
;;

let rec take_num bytes numBytes pos =
  if pos >= Bytes.length bytes
  then (
    let new_bytes = Stdlib.Effect.perform Read in
    if Bytes.length new_bytes = 0
    then Num (num_from_bytes numBytes), bytes, pos
    else take_num new_bytes numBytes 0)
  else (
    let c = Bytes.get bytes pos in
    if is_num c
    then take_num bytes (c :: numBytes) (pos + 1)
    else Num (num_from_bytes numBytes), bytes, pos)
;;

let rec get_token bytes pos =
  (* Stdio.printf "get_token bytes: '%s'\n" (bytes|>Bytes.to_string); *)
  let pos = eat_whitespace bytes pos in
  if pos >= Bytes.length bytes
  then (
    let new_bytes = Stdlib.Effect.perform Read in
    (* Stdio.printf "get_token:new bytes: '%s'\n" (new_bytes|>Bytes.to_string); *)
    if Bytes.length new_bytes = 0
    then (
      let () = Stdlib.Effect.perform End_of_input in
      raise End_of_file)
    else get_token new_bytes 0)
  else (
    (* Stdio.printf "pos before match: %d\n" pos; *)
    (* Stdio.printf "byte before match: '%c'\n" (Bytes.get bytes pos); *)
    match Bytes.get bytes pos with
    | '[' -> Open, bytes, pos + 1
    | ']' -> Close, bytes, pos + 1
    | ',' -> Sep, bytes, pos + 1
    | c when is_num c ->
      let num_token, bytes ,pos= take_num bytes [ c ] (pos + 1) in
      num_token, bytes, pos 
    | c -> raise (TokenizeErr (Printf.sprintf "bad char: %c" c)))
;;

(* Effectful parser *)
let rec parse_list bytes pos =
  let token, bytes, new_pos = get_token bytes pos in
  (* Stdio.printf "token: %s\n" (token|>show_token); *)
  match token with
  | Close -> [], bytes, new_pos
  | Num n ->
    let rest, bytes, final_pos = parse_after_value bytes new_pos in
    NumT n :: rest, bytes, final_pos
  | Open ->
    let inner, bytes, after_inner = parse_list bytes new_pos in
    let rest, bytes, final_pos = parse_after_value bytes after_inner in
    ListT inner :: rest, bytes, final_pos
  | Sep -> parse_list bytes new_pos
  | _ -> raise (InvalidStart (Printf.sprintf "Invalid start: %s" (token|>show_token)))

and parse_after_value bytes pos =
  let token, bytes, new_pos = get_token bytes pos in
  match token with
  | Close -> [], bytes, new_pos
  | Sep -> parse_list bytes new_pos
  | _ -> raise MissingAfterVal
;;

let parse bytes pos=
  let token,bytes, pos = get_token bytes pos in
  (* Stdio.printf "token: %s\n" (token|>show_token); *)
  match token with
  | Num n -> NumT n ,bytes ,pos
  | Open ->
    let result, bytes,pos = parse_list bytes pos in
    ListT result ,bytes ,pos
  | _ -> raise (InvalidNextToken (Printf.sprintf "Invalid after parse: %s" (token|>show_token)))
;;

let rec parseLoop handler state bytes pos =
  let (parsed,bytes,pos) = parse bytes pos in
  let state=handler state parsed in
  match get_token bytes pos with
  | Sep ,bytes,pos -> parseLoop handler state bytes pos
  | Close ,bytes,pos -> 
    (* Stdio.printf "parseLoop: Close\n"; *)
    state
  | token,bytes,pos -> raise (InvalidNextToken (Printf.sprintf "invalid after parse token: %s" (token|>show_token)))

let startParse handler state  =
  parseLoop handler state 

  

(* Handler *)
let handle_parser f (read_next : unit -> bytes) =
  let handler =
    Stdlib.Effect.Deep.
      { retc = (fun x -> x)
      ; exnc = (fun e -> raise e)
      ; effc =
          (fun (type a) (eff : a Stdlib.Effect.t) ->
            match eff with
            | Read ->
              Some
                (fun (k : (a, _) Stdlib.Effect.Deep.continuation) ->
                  let new_bytes = read_next () in
                  (* Stdio.printf "new_bytes: '%s'\n" (new_bytes|>Bytes.to_string); *)
                  Stdlib.Effect.Deep.continue k new_bytes )
            | End_of_input ->
              Some
                (fun (k : (a, _) Stdlib.Effect.Deep.continuation) ->
                  Stdlib.Effect.Deep.continue k ())
            | _ -> None)
      }
  in
  Stdlib.Effect.Deep.match_with (f (Bytes.create 0)) (0) handler
;;

(* Example usage:
   let result = handle_parser (fun bytes pos -> parse bytes) read_next_fn
*)

(* Tests *)
let%test "basic number parsing" =
  let input = Bytes.of_string "42" in
  let read_count = ref 0 in
  let read_next () = 
    Int.incr read_count;
    Stdio.printf "read_count: %d\n" !read_count;

    if !read_count = 1 then input else Bytes.create 0
  in
  match handle_parser parse  read_next with
  | NumT 42 ,_,_ -> true
  | _ -> false
;;

let%test "basic list parsing" =
  let input = Bytes.of_string "[1,2,3]]" in
  let read_count = ref 0 in
  let read_next () = 
    Int.incr read_count;
    if !read_count = 1 then input else Bytes.create 0
  in
  match handle_parser parse  read_next with
  | ListT [NumT 1; NumT 2; NumT 3] ,_,_ -> true
  | _ -> false
;;




let%test "nested list parsing" =
  let input = Bytes.of_string "[1,[2,3],4]" in
  let read_count = ref 0 in
  let read_next () = 
    Int.incr read_count;
    if !read_count = 1 then input else Bytes.create 0
  in
  match handle_parser parse  read_next with
  | ListT [NumT 1; ListT [NumT 2; NumT 3]; NumT 4] ,_,_ -> true
  | _ -> false
;;

let%test "streaming input parsing" =
  let input1 = Bytes.of_string "[1,2" in
  let input2 = Bytes.of_string ",3]" in
  let read_count = ref 0 in
  let read_next () = 
    Int.incr read_count;
    match !read_count with
    | 1 -> input1
    | 2 -> input2
    | _ -> Bytes.create 0
  in
  match handle_parser parse  read_next with
  | ListT [NumT 1; NumT 2; NumT 3] ,_,_ -> true
  | _ -> false
;;

let%test "empty list parsing" =
  let input = Bytes.of_string "[]" in
  let read_count = ref 0 in
  let read_next () = 
    Int.incr read_count;
    if !read_count = 1 then input else Bytes.create 0
  in
  match handle_parser parse read_next with
  | ListT [] ,_,_ -> true
  | _ -> false
;;

let%test "deeply nested list parsing" =
  let input = Bytes.of_string "[1,[2,[3,[4]]]]" in
  let read_count = ref 0 in
  let read_next () = 
    Int.incr read_count;
    if !read_count = 1 then input else Bytes.create 0
  in
  match handle_parser parse read_next with
  | ListT [NumT 1; ListT [NumT 2; ListT [NumT 3; ListT [NumT 4]]]] ,_,_ -> true
  | _ -> false
;;

let%test "whitespace handling" =
  let input = Bytes.of_string "  [  1 , 2  ,  3  ]  " in
  let read_count = ref 0 in
  let read_next () = 
    Int.incr read_count;
    if !read_count = 1 then input else Bytes.create 0
  in
  match handle_parser parse read_next with
  | ListT [NumT 1; NumT 2; NumT 3] ,_,_ -> true
  | _ -> false
;;

let test_error_case input expected_exn =
  (* try *)
    let read_count = ref 0 in
    let read_next () = 
      Int.incr read_count;
      if !read_count = 1 then Bytes.of_string input else Bytes.create 0
    in
    let _ = handle_parser parse read_next in
    false
  (* with
  | e when Poly.equal e expected_exn -> true
  | _ -> false *)
;;

let%test "invalid start error" =
  test_error_case "}" (InvalidStart "}")
;;

let%test "missing after value error" =
  test_error_case "[1 2]" (TokenizeErr "bad char: 2")
;;

let%test "tokenize error" =
  test_error_case "[1,@]" (TokenizeErr "bad char: @")
;;

let%test "streaming with multiple chunks" =
  let chunks = ["[1,"; "2,"; "3,"; "4]"] in
  let current = ref 0 in
  let read_next () =
    if !current >= List.length chunks
    then Bytes.create 0
    else (
      let chunk = Bytes.of_string (List.nth_exn chunks !current) in
      Int.incr current;
      chunk)
  in
  match handle_parser parse read_next with
  | ListT [NumT 1; NumT 2; NumT 3; NumT 4] ,_,_ -> true
  | _ -> false
;;

let%test "basic list parsing2" =
  Stdio.printf "=======basic list parsing2=========\n";
  let input = Bytes.of_string "1,[1,2,3]]" in
  let read_count = ref 0 in
  let read_next () = 
    Int.incr read_count;
    if !read_count = 1 then input else Bytes.create 0
  in
  match handle_parser (startParse (fun state value -> 
    Stdio.printf "value: %s\n" (value|>show_parse_result);
    state+1) 0) read_next with
  | 2 -> true
  | _ -> false
;;