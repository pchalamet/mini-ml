module Main

open AbstractSyntaxTree

open FSharp.Text.Lexing
open System.IO

let debugToken lexbuf =
    let res = Lexer.token lexbuf
    printfn "token: %A" res
    res

let parse txt =
  txt
  |> LexBuffer<_>.FromString
  |> Parser.File debugToken
 

[<EntryPoint>]
let main argv =
    let content = File.ReadAllText ("Samples/Test2.mfs")
    let ast = content |> parse
    ast |> printfn "%A"

    0
