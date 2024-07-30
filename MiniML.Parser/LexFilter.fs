module LexFilter
open FSharp.Text.Lexing
open MiniML.Parser

type LexBuf = LexBuffer<char>
let mutable indent_levels = [ 0 ]
let mutable read_queue = []
let mutable last_token_was_newline = false

let rec read read_one (lexbuf: LexBuf) =
    let ret_token =
        match read_queue with
        | [] ->
            read_one lexbuf
        | head :: tail ->
            read_queue <- tail
            head

    match ret_token with
    | NEWLINE ->
        last_token_was_newline <- true
        read read_one lexbuf
    | EOF ->
        if 0 < indent_levels.Head then
            indent_levels <- indent_levels.Tail
            read_queue <- EOF :: read_queue
            lexbuf.IsPastEndOfStream <- false
            DEDENT
        else
            lexbuf.IsPastEndOfStream <- true
            ret_token
    | _ ->
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
                read_queue <- ret_token :: read_queue
                last_token_was_newline <- false
                NEWLINE
        // same line emit token
        else
            ret_token
