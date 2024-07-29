module MiniML.Lexer

open MiniML.Parser  // we need the terminal tokens from the Parser
open FSharp.Text.Lexing/// Rule read_one
val read_one: lexbuf: LexBuffer<char> -> token
/// Rule singleLineComment
val singleLineComment: lexbuf: LexBuffer<char> -> token
