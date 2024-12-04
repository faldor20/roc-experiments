open Base

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

type parse_error = 
  | InvalidStart of token list
  | MissingAfterStart of token list
  | MissingAfterVal of token list
  | TokenizeErr
  | LeftoverTokens
  [@@deriving show]

type tokenize_error =
  | NeedMore of (bytes -> int -> (token * int, tokenize_error) Result.t)
  | TokenizeErr

type read_result = 
  | End
  | More of bytes

(* Updated helper functions for bytes *)
let is_whitespace = function
  | ' ' | '\n' | '\r' | '\t' -> true
  | _ -> false

let rec eat_whitespace bytes pos =
  if pos >= Bytes.length bytes then pos
  else if is_whitespace (Bytes.get bytes pos) then
    eat_whitespace bytes (pos + 1)
  else pos

let is_num c = 
  Char.(c >= '0' && c <= '9')

let num_from_bytes numBytes =
  (* Stdio.printf "NumBytes: %s\n" (numBytes |> List.map ~f:(fun c -> Char.to_string c) |> String.concat ~sep:","); *)
  numBytes
  |> List.fold ~init:(0,0) ~f:(fun (acc, multiplier) c -> 
    let digit = Char.to_int c -0x30  in
    (* Stdio.printf "Digit: %d\n" digit; *)
    (acc + digit * multiplier, multiplier * 10))
  |> fst

(* Updated tokenizer *)
let rec take_num bytes numBytes pos =
  if pos >= Bytes.length bytes then
    Error (NeedMore ((fun bytes pos -> take_num bytes numBytes pos)))
  else
    let c = Bytes.get bytes pos in
    if is_num c then
      take_num bytes (c::numBytes) (pos + 1)
    else
      Ok (Num (num_from_bytes numBytes ), pos)

let rec get_token bytes pos =
  (* Stdio.printf "Pos: %d\n" pos; *)
  (* Stdio.printf "Bytes: %s\n" (Bytes.to_string (Bytes.sub bytes ~pos ~len:(Bytes.length bytes - pos))); *)
  if pos >= Bytes.length bytes then
  (
    (* (Stdio.printf "NeedMore\n" ; *)
    Error (NeedMore( get_token  )))
  else
    match Bytes.get bytes pos with
    | '[' -> Ok (Open, pos + 1)
    | ']' -> Ok (Close, pos + 1)
    | ',' -> Ok (Sep, pos + 1)
    | c when is_num c -> take_num bytes [c] (pos + 1)
    | _ -> 
      (* Stdio.printf "Error: %c\n" e; *)
      Error TokenizeErr

type tokenize_state = {
  tokens: token list;
  depth: int;
  pos: int;
  continue: bytes -> int -> (token * int, tokenize_error) Result.t;
}

let rec tokenize_loop state bytes =
  (* TODO: This could cuase whitespace to be eaten in the middle of a number *)
  let pos = eat_whitespace bytes state.pos in
  (* Stdio.printf "Pos: %d\n" pos; *)
  match state.continue bytes pos with
  | Error ((NeedMore _)as more) -> Error (more, state)
  | Error TokenizeErr -> Error (TokenizeErr, state)
  | Ok (token, pos) ->
      let new_depth = match token with
        | Open -> state.depth + 1
        | Close -> state.depth - 1
        | _ -> state.depth
      in
      (* Stdio.printf "Token: %s\n" (show_token token); *)
      (* Stdio.printf "Depth: %d\n" new_depth; *)
      let new_tokens = state.tokens @ [token] in
      let new_state = { 
        tokens = new_tokens;
        depth = new_depth;
        pos ;
        continue = get_token;
      } in
      if new_depth <= 0 then
        Ok (new_tokens,  pos)
      else
        tokenize_loop new_state bytes

let tokenize bytes read_more =
  let rec loop state bytes=
    match tokenize_loop state bytes with
    | Error (TokenizeErr, _) -> Error TokenizeErr
    | Error (NeedMore(continue), state) ->
        (* You could get super fancy and use a ring buffer here, you parse untill you get stuck and need more and then you return the current index and then copy then tell it to write the new data after the current part and then start parsin again*)
        (match read_more bytes with
         | End -> Ok (state.tokens, End)
         | More bytes -> 
            if Bytes.length bytes =0 then
              Ok (state.tokens, End)
            else (* Stdio.printf "New bytes: %s\n" (Bytes.to_string bytes); *)
             loop {state with continue; pos = 0} bytes)
    | Ok (tokens, pos) ->

        let remaining = Bytes.sub bytes ~pos ~len:(Bytes.length bytes - pos) in
        Ok (tokens, More remaining)
  in
  let initial_state = {
    tokens = [];
    depth = 0;
    pos = 0;
    continue = get_token;
  } in
  loop initial_state bytes

(* Parser *)
type parse_state = Start | ListStart | Val | SepS
[@@deriving show]

let rec parse last state tokens = 
  (* Stdio.printf "Last: %s\n" (show_parse_state last); *)
    match last with
    | Val -> (
      match tokens with
      | Sep :: rest2 -> parse SepS state rest2
      | Close :: rest -> Ok (ListT state, rest)
      | _ -> Error (MissingAfterVal tokens)
    )
    | ListStart | SepS -> (
      match tokens with
      | Close :: rest -> Ok (ListT state, rest)
      | Num n :: rest -> parse Val (state @ [NumT n]) rest
      | Open :: rest -> (
        match parse ListStart [] rest with
        | Error e -> Error e
        | Ok (val_, rest2) -> parse Val (state @ [val_]) rest2
      )
      | _ -> Error (MissingAfterStart tokens)
    )
    | Start -> (
      match tokens with
      | Num n :: rest -> Ok (NumT n, rest)
      | Open :: rest -> (
        match parse ListStart [] rest with
        | Error e -> Error e
        | Ok (val_, rest2) -> Ok (val_, rest2)
      )
      | _ -> Error (InvalidStart tokens)
    )


let parse_tokens tokens =
  parse Start [] tokens

let%test "basic parsing" =
  let input = Bytes.of_string "[12,122,[300]]" in
  let read_count = ref 0 in
  let read_more _ = 
    Int.incr read_count;
    End
  in
  match tokenize input read_more with
  | Error _ -> false
  | Ok (tokens, _) -> 

      match parse_tokens tokens with
      | Ok _ -> true
      | Error _ -> false

let%test "streaming parsing" =
  let input1 = Bytes.of_string "[12,122,[30" in
  let input2 = Bytes.of_string "0]]" in
  let read_count = ref 0 in
  let read_more _ = 
    Int.incr read_count;
    if !read_count = 1 then More input2
    else End
  in
  match tokenize input1 read_more with
  | Error _ -> false 
  | Ok (tokens, _) -> 
      match parse_tokens tokens with
      | Ok _ -> true
      | Error _ -> false

let value_stream bytes init_state handle read_more =
  let rec loop state inp =
    match tokenize inp read_more with
    | Ok ([Close], End) -> Ok state
    | Ok ([Close], More _) -> Ok state
    | Ok ([Sep], End) -> Ok state
    | Ok ([Sep], More rest) -> loop state rest
    | Ok (tokens, rest) ->

        (* Stdio.printf "Tokens: %s\n" ( tokens|> List.map ~f:show_token |> String.concat ~sep:" "); *)
        (match parse_tokens tokens with
         | Error e -> Error e
         | Ok (_, _::_) -> Error LeftoverTokens
         | Ok (value, []) ->
             let new_state = handle state value in
             
             (match rest with
              | End -> Ok state
              | More rest2 -> loop new_state rest2))
    | Error _ -> Error TokenizeErr
  in
  loop init_state bytes
