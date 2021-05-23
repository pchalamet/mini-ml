module Main

open FSharp.Text.Lexing
open System.IO

let parse txt =
    let lexbuf = LexBuffer<_>.FromString txt
    let lexFilter = LexFilter.LexFilter()
    Parser.File lexFilter.NextToken lexbuf


[<EntryPoint>]
let main argv =
    let content = File.ReadAllText ("Samples/Test1.mfs")
    let ast = content |> parse
    ast |> printfn "%A"

    0
