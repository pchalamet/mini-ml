module Main
open System.IO


[<EntryPoint>]
let main argv =
    let content = File.ReadAllText ("Samples/Test1.mfs")
    let ast = content |> MiniML.FrontEnd.parse
    ast |> printfn "%A"

    0
