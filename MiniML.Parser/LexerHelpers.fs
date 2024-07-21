module MiniML.LexerHelpers
open MiniML.Parser
open FSharp.Text.Lexing

type LexBuf = LexBuffer<char>
let lexeme = LexBuffer<_>.LexemeString

let mutable num_brackets = 0
let mutable indent_levels = [ 0 ]
let mutable read_queue = []
let mutable last_token_was_newline = true


exception ParseException of string


let keyword_table = Map [
    "module", MODULE
    "let", LET
    "if", IF
    "then", THEN
    "else", ELSE
    "match", MATCH
    "with", WITH
]

let count_ws (str : string) : int =
    let sum_over acc c =
        let sz =
            match c with
            | '\t' -> 4
            | ' ' -> 1
            | _ -> 0
        acc + sz

    str |> Seq.fold sum_over 0

let pop_and_count n =
    let rec iter acc =
        match indent_levels with
        | head :: tail when head > n ->
            indent_levels <- tail
            iter (acc+1)
        | head :: _ when head = n -> acc
        | _ -> failwith "indent error"
    iter 0

let enqueue_dedents new_level =
    let num_dedents = pop_and_count new_level
    let dedents = List.init (num_dedents - 1) (fun _ -> DEDENT)
    dedents @ read_queue


let curr_col (lexbuf: LexBuf) =
    lexbuf.StartPos.Column

let is_start_of_line lexbuf =
    curr_col lexbuf = 0

let newline (lexbuf: LexBuf) =
    lexbuf.EndPos <- lexbuf.EndPos.NextLine

let on_enter_bracket token =
    num_brackets <- num_brackets + 1
    token

let on_leave_bracket token =
    num_brackets <- num_brackets - 1
    token



let on_newline read_one (lexbuf: LexBuf) =
    newline lexbuf
    if num_brackets = 0 then
        if is_start_of_line lexbuf then
            read_one lexbuf
        else
            NEWLINE
    else
        read_one lexbuf

let on_id (lexbuf: LexBuf) =
    let word = LexBuf.LexemeString lexbuf
    let ret_token =
        match keyword_table |> Map.tryFind word with
        | Some token -> token
        | _ -> ID word

    if is_start_of_line lexbuf && indent_levels.Head <> 0 then
        read_queue <- ret_token :: enqueue_dedents 0
        DEDENT
    else
        ret_token

let on_whitespace read_one (lexbuf: LexBuf) =
    if is_start_of_line lexbuf && num_brackets = 0 then
        let count = count_ws (lexeme lexbuf)
        let topstack = indent_levels.Head
        if count > topstack then
            indent_levels <- count :: indent_levels
            INDENT
        elif count < topstack then
            read_queue <- enqueue_dedents count
            DEDENT
        else
            read_one lexbuf
    else
        read_one lexbuf

let on_eof () =
    if indent_levels.Head > 0 then
        read_queue <- EOF :: enqueue_dedents 0
        DEDENT
    else
        EOF


let rec read read_one lexbuf =
    let ret_token =
        match read_queue with
        | [] ->
            read_one lexbuf
        | head :: tail ->
            read_queue <- tail
            head

    match ret_token with
    | NEWLINE ->
        if last_token_was_newline then
            read read_one lexbuf
        else
            last_token_was_newline <- true
            NEWLINE
    | _ ->
        last_token_was_newline <- false
        ret_token
