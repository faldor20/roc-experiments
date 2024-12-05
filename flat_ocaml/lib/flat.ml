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
  | TokenizeErr of string 
  | InvalidAfterParse of char
  | LeftoverTokens
  [@@deriving show]

type tokenize_error =
  | NeedMore of (bytes -> int -> (token * int, tokenize_error) Result.t)
  | TokenizeErr of string

type read_result = 
  | End
  | More of bytes

type tokenizeRemainder = 
  | Done
  | Leftover of (bytes*int)

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
  |> List.fold ~init:(0,1) ~f:(fun (acc, multiplier) c -> 
    let digit = Char.to_int c - 0x30  in
    (* Stdio.printf "Digit: %d\n" digit; *)
    (acc + digit * multiplier, multiplier * 10))
  |> fst

(* Updated tokenizer *)
let rec take_num bytes numBytes pos =
  (* Stdio.printf "NumBytes: %s\n" (numBytes |> List.map ~f:(fun c -> Char.to_string c) |> String.concat ~sep:","); *)
  if pos >= Bytes.length bytes then
    Error (NeedMore ((fun bytes pos -> take_num bytes numBytes pos)))
  else
    let c = Bytes.get bytes pos in
    if is_num c then
      take_num bytes (c::numBytes) (pos + 1)
    else
(
      Ok (Num (num_from_bytes numBytes ), pos)
      )

let rec get_token bytes pos =
  let pos = eat_whitespace bytes pos in
  if pos >= Bytes.length bytes then
    Error (NeedMore( get_token ))
  else
  begin
    (* (Stdio.printf "byte: %c\n" (Bytes.get bytes pos)); *)
    match Bytes.get bytes pos with
    | '[' -> Ok (Open, pos + 1)
    | ']' -> Ok (Close, pos + 1)
    | ',' -> Ok (Sep, pos + 1)
    | c when is_num c -> take_num bytes [c] (pos + 1)
    | c -> 
      Error (TokenizeErr (Printf.sprintf "bad char: %c" c))
    end

type tokenize_state = {
  tokens: token list;
  depth: int;
  pos: int;
  continue: bytes -> int -> (token * int, tokenize_error) Result.t;
  lastToken:bool
}
 [@@deriving show]

let getDepth depth token =
      match token with
        | Open -> depth + 1
        | Close -> depth - 1
        | _ -> depth

let rec tokenize_loop state bytes =
  (* TODO: This could cuase whitespace to be eaten in the middle of a number *)
  (* Stdio.printf "Pos: %d\n" pos; *)
  match state.continue bytes state.pos with
  | Error ((NeedMore _)as more) ->
     Error (more, state)
  | Error TokenizeErr s -> Error (TokenizeErr s, state)
  | Ok (token, pos) ->
    if state.lastToken then
    (
    (*check the next token to see if it indicates the end or a comma indicatin more data*)
      match token with
        |Sep->
          Ok (state.tokens, pos,false)
        |Close-> 
          Ok (state.tokens, pos,true)
        |_-> Error (TokenizeErr(show_tokenize_state state),state)
      )
    else begin
    
      let new_depth = getDepth state.depth token in
      let new_tokens = token::state.tokens  in
let new_state =       

        { 
          tokens = new_tokens;
          depth = new_depth;
          pos;
          continue = get_token;
          lastToken= (new_depth=0)
        } 
        in
          tokenize_loop new_state bytes
      end

let tokenize bytes pos read_more =
  let rec loop state bytes=
    (* Stdio.printf "bytes%d:%s\n" (bytes|>Bytes.length)(bytes|>Bytes.to_string); *)
    match tokenize_loop state bytes with
    | Error (TokenizeErr s, _) -> Error (TokenizeErr ( s))
    | Error (NeedMore(continue), state) ->
        (* You could get super fancy and use a ring buffer here, you parse untill you get stuck and need more and then you return the current index and then copy then tell it to write the new data after the current part and then start parsin again*)
        (match read_more bytes with
         | End -> 
           Ok (state.tokens|>List.rev, Done)
         | More newBytes -> 
            if Bytes.length newBytes = 0 then
            (
              Ok (state.tokens|>List.rev, Done)
              )
            else 

            (
              let state={state with continue; pos = 0;} in
              loop state newBytes
            )
          )
    | Ok (tokens, pos,isLast) ->
        Ok (tokens|>List.rev, if isLast then Done else Leftover(bytes, pos))
  in
  let initial_state = {
    tokens = [];
    depth = 0;
    pos; 
    lastToken=false;
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
let print_tokens tokens=
      (* Stdio.printf "Tokens: %s\n" (tokens |> List.map ~f:show_token |> String.concat ~sep:","); *)
      ()

let%test "basic parsing" =
  let input = Bytes.of_string "[12,122,[300]]" in
  let read_count = ref 0 in
  let read_more _ = 
    Int.incr read_count;
    End
  in
  match tokenize input 0 read_more with
  | Error _ -> false
  | Ok (tokens, _) -> 
      match parse_tokens tokens with
      | Ok _ -> true
      | Error _ -> 
        print_tokens tokens;
        false

let%test "streaming parsing" =
  let input1 = Bytes.of_string "[12,122,[30" in
  let input2 = Bytes.of_string "0]]" in
  let read_count = ref 0 in
  let read_more _ = 
    Int.incr read_count;
    if !read_count = 1 then More input2
    else End
  in
  match tokenize input1 0 read_more with
  | Error _ -> false 
  | Ok (tokens, _) -> 
      match parse_tokens tokens with
      | Ok _ -> true
      | Error _ -> false

(* type whatNext= *)
  (* Next|End|Invalid *)

(* let whatNext   bytes= *)
  (* match Bytes.get bytes 0 with *)
  (* |','-> Next *)
  (* |']'-> End *)
  (* |_->Invalid *)


let value_stream bytes init_state handle read_more =

  let rec loop state inp pos =
      match tokenize inp pos read_more with
      | Ok (tokens, rest) ->
      (* Stdio.printf "Tokens: %s\n" (tokens |> List.map ~f:show_token |> String.concat ~sep:","); *)
          (match parse_tokens tokens with
           | Error e -> Error e
           | Ok (_, _::_) -> Error LeftoverTokens
           | Ok (value, []) ->
               let new_state = handle state value in
               (match rest with
                | Done -> Ok state
                | Leftover (bytes, pos )-> 
                    (*read the next byte and decide whether to end or not*)
                   loop new_state bytes pos
                ))
      | Error (TokenizeErr s) -> Error (TokenizeErr s )
      
  in
  loop init_state bytes 0
