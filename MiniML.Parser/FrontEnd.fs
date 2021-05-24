module MiniML.FrontEnd
open FSharp.Text.Lexing

let parse txt =
    let lexbuf = LexBuffer<_>.FromString txt
    let lexFilter = LexFilter.LexFilter()
    Parser.File lexFilter.NextToken lexbuf
