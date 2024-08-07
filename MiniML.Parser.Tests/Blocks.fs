module MiniML.Parser.Tests
open NUnit.Framework
open FSharp.Text.Lexing


[<Test>]
let check_compound () =
    let tokens = [
        MODULE; ID "toto"; NEWLINE

        NUMBER 10m; NEWLINE
        IF; ID "a"; EQUAL; NUMBER 10m; THEN; NUMBER 10m

        EOF
    ]

    let mutable index = 0
    let lexer _ =
        let ret_token = tokens[index]
        index <- index + 1
        printfn $"#### {ret_token}"
        ret_token

    let lexbuf = LexBuffer<_>.FromString "tagada"
    let result = MiniML.Parser.File lexer lexbuf
    printfn $"{result}"



// [<Test>]
// let ``Single statement is valid`` () =
//     let file = """module toto 
// let a = 42"""

//     let expected = Module ("toto", [ Let ("a", [ Return (Number 42.0m) ]) ])
//     file |> MiniML.FrontEnd.parse |> should equal expected

// [<Test>]
// let ``multiple statements in same block are valid`` () =
//     let file = """module toto 
// let a = 42
// let b = 666"""

//     let expected = Module ("toto", [ Let ("a", [ Return (Number 42.0m) ])
//                                      Let ("b", [ Return (Number 666.0m) ]) ])
//     file |> MiniML.FrontEnd.parse |> should equal expected

// [<Test>]
// let ``multiple statements in same block must be aligned`` () =
//     let file = """module toto 
// let a = 42
//  let b = 666"""

//     (fun () -> MiniML.FrontEnd.parse file |> ignore) |> should throw typeof<MiniML.LexerHelpers.ParseException>

// [<Test>]
// let ``if then else with single block is valid``() =
//     let file = """module toto 

// if a == 10 then 42
// else 666
// """

//     let expected = Module ("toto", [ IfThenElse (Function (Equal, Symbol "a", Number 10.0m),
//                                                  [ Return (Number 42.0m) ], 
//                                                  [ Return (Number 666.0m) ] ) ] )
//     file |> MiniML.FrontEnd.parse |> should equal expected


// [<Test>]
// let ``parent else is associated with corresponding if``() =
//     let file = """module toto 

// if a == 10 then
//     if b == 11 then 42 
// else 666
// """

//     let expected = Module ("toto", [ IfThenElse (Function (Equal, Symbol "a", Number 10.0m), 
//                                                  [ IfThenElse (Function (Equal, Symbol "b", Number 11.0m),  
//                                                                [ Return (Number 42.0m) ],
//                                                                []) ],
//                                                  [ Return (Number 666.0m) ] ) ] )
//     file |> MiniML.FrontEnd.parse |> should equal expected



// [<Test>]
// let ``nested else is associated with corresponding if``() =
//     let file = """module toto 

// if a == 10 then
//     if b == 11 then 42 
//     else 666
// """

//     let expected = Module ("toto", [ IfThenElse (Function (Equal, Symbol "a", Number 10.0m), 
//                                                  [ IfThenElse (Function (Equal, Symbol "b", Number 11.0m),  
//                                                                [ Return (Number 42.0m) ],
//                                                                [ Return (Number 666.0m) ]) ],
//                                                  [ ] ) ] )
//     file |> MiniML.FrontEnd.parse |> should equal expected


// [<Test>]
// let ``parent else is not correctly indented after nested block``() =
//     let file = """module toto 

// if a == 10 then
//     if b == 11 then 42 
//   else 10
// """

//     (fun () -> file |> MiniML.FrontEnd.parse |> ignore) |> should throw typeof<MiniML.LexerHelpers.ParseException>


// [<Test>]
// let ``nested else is not correctly indented``() =
//     let file = """module toto 

// if a == 10 then
//     if b == 11 then 42 
//       else 10
// """

//     (fun () -> file |> MiniML.FrontEnd.parse |> ignore) |> should throw typeof<MiniML.LexerHelpers.ParseException>
