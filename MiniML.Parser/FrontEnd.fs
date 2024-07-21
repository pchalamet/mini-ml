module MiniML.FrontEnd
open FSharp.Text.Lexing


let inline private dumpLexer lexer (lexbuff: LexBuffer<char>) =
    let token = lexer lexbuff
    printfn $"TOKEN = {token}"
    token



let parse txt =
    let lexbuf = LexBuffer<_>.FromString txt
    let lexer = LexerHelpers.read Lexer.read_one
    Parser.File (dumpLexer lexer) lexbuf
