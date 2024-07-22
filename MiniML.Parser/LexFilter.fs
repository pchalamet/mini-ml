module LexFilter
open FSharp.Text.Lexing
open MiniML.Parser

type LexBuf = LexBuffer<char>
let lexeme = LexBuffer<_>.LexemeString

let mutable indent_levels = [ 0 ]
let mutable read_queue = []
let mutable last_token_was_newline = true


let is_start_of_line (lexbuf: LexBuf) =
    lexbuf.StartPos.Column = 0

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
    let dedents = List.init (num_dedents+1) (fun _ -> DEDENT)
    dedents @ read_queue

let rec read read_one (lexbuf: LexBuf) =
    let ret_token =
        match read_queue with
        | [] -> read_one lexbuf
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
    | EOF ->
        if 0 < indent_levels.Head then
            indent_levels <- indent_levels.Tail
            read_queue <- ret_token :: read_queue
            DEDENT
        else
            ret_token
    | _ ->
        // NEWLINE has been emitted before and current token is not NEWLINE
        if last_token_was_newline then
            // shall we INDENT ?
            if indent_levels.Head < lexbuf.StartPos.Column then
                indent_levels <- lexbuf.StartPos.Column :: indent_levels
                read_queue <- ret_token :: read_queue
                last_token_was_newline <- false
                INDENT
            // shall we DEDENT ?
            elif lexbuf.StartPos.Column < indent_levels.Head then
                indent_levels <- indent_levels.Tail
                read_queue <- ret_token :: read_queue
                DEDENT
            // same indent emit token
            else
                last_token_was_newline <- false
                ret_token
        // same line emit token
        else
            ret_token
