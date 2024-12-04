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
  | NeedMore of (char list -> (token * char list, tokenize_error) Result.t)
  | TokenizeErr

type read_result = 
  | End
  | More of char list

(* Tokenizer *)
let is_whitespace = function
  | ' ' | '\n' | '\r' | '\t' -> true
  | _ -> false

let rec eat_whitespace = function
  | c :: rest when is_whitespace c -> eat_whitespace rest
  | chars -> chars

let is_num c = 
  let c_int = Char.to_int c in
  c_int >= Char.to_int '0' && c_int <= Char.to_int '9'

let num_from_utf8 nums =
  List.fold_right nums ~init:(0, 1) ~f:(fun num_char (acc, idx) ->
    let digit = Char.to_int num_char - Char.to_int '0' in
    ((digit * idx + acc), (idx * 10))
  ) |> fst

let%test "num_from_utf8 basic" = 
  num_from_utf8 ['2'; '8'] = 28

let%test "num_from_utf8 larger" = 
  num_from_utf8 ['2'; '8'; '8'; '0'] = 2880

let rec take_num num_chars = function
  | [] -> Error (NeedMore (take_num num_chars))
  | c :: rest when is_num c -> 
      take_num (c :: num_chars) rest
  | rest -> 
      if not (List.is_empty num_chars) then
        Ok (Num (num_from_utf8 (List.rev num_chars)), rest)
      else 
        Error TokenizeErr

let rec get_token = function
  | [] -> Error (NeedMore get_token)
  | '[' :: rest -> Ok (Open, rest)
  | ']' :: rest -> Ok (Close, rest)
  | ',' :: rest -> Ok (Sep, rest)
  | input when is_num (List.hd_exn input) -> take_num [] input
  | _ -> Error TokenizeErr

type tokenize_state = {
  tokens: token list;
  depth: int;
  continue: char list -> (token * char list, tokenize_error) Result.t;
}

let rec tokenize_loop state input =
  match state.continue (eat_whitespace input) with
  | Error (NeedMore continue) -> Error (NeedMore continue, state)
  | Error TokenizeErr -> Error (TokenizeErr, state)
  | Ok (token, rest) ->
      let new_depth = match token with
        | Open -> state.depth + 1
        | Close -> state.depth - 1
        | _ -> state.depth
      in
      let new_tokens = state.tokens @ [token] in
      let new_state = { 
        tokens = new_tokens;
        depth = new_depth;
        continue = get_token;
      } in
      if new_depth = 0 then
        Ok (new_tokens, rest)
      else
        tokenize_loop new_state rest

let tokenize input read_more =
  let rec loop state input =
    match tokenize_loop state input with
    | Error (TokenizeErr, _) -> Error TokenizeErr
    | Error (NeedMore continue, state) ->
        (match read_more () with
         | End -> Ok (state.tokens, End)
         | More rest -> loop {state with continue} rest)
    | Ok (tokens, rest) -> Ok (tokens, More rest)
  in
  let initial_state = {
    tokens = [];
    depth = 0;
    continue = get_token;
  } in
  loop initial_state input

(* Parser *)
type parse_state = Start | ListStart | Val | SepS

let rec parse last state = function
  | [] -> Error (MissingAfterVal [])
  | tokens -> 
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
  let input = String.to_list "[12,122,[300]]" in
  let read_count = ref 0 in
  let read_more () = 
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
  let input1 = String.to_list "[12,122,[30" in
  let input2 = String.to_list "0]]" in
  let read_count = ref 0 in
  let read_more () = 
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

let value_stream input init_state handle read_more =
  let rec loop state inp =
    match tokenize inp read_more with
    | Ok ([Close], End) -> Ok state
    | Ok ([Sep], End) -> Ok state
    | Ok ([Sep], More rest) -> loop state rest
    | Ok (tokens, rest) ->
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
  loop init_state input
