module LexFilter
open FSharp.Text.Lexing

let mutable guideStack: (int * int) list = [ 0, 0 ]

let guide() = guideStack.Head

let guideCol() = guideStack.Head |> fst

let tryGuide() =
    guideStack |> List.tryHead

let pushGuide guide =
    // printfn "Pushing guide %d" guide
    guideStack <- guide :: guideStack

let popGuide() = 
    match guideStack with
    | head :: tail -> guideStack <- tail
                    //   printfn "Poping guide %d" head
                      head
    | _ -> failwith "no guide available"


let mutable tokenStack: (Parser.token * int * int) list = List.empty 

let pushToken token =
    // printfn "Pushing token %A" token
    tokenStack <- token :: tokenStack

let popToken() = 
    match tokenStack with
    | head :: tail -> tokenStack <- tail
                    //   printfn "Poping token %A" head
                      head
    | _ -> failwith "no token available"


let getToken (lexbuf: LexBuffer<char>) =
    Lexer.token lexbuf, lexbuf.StartPos.Column, lexbuf.StartPos.Line

let lexFilter (lexbuf: LexBuffer<char>) =
    // printfn "*** lexFilter"
    let currToken, currCol, currRow = if tokenStack |> List.isEmpty |> not then popToken()
                                      else
                                          let currToken, currTokenCol, currTokenRow = getToken lexbuf
                                          //    printfn "token %A (%d, %d)" currToken currTokenCol currTokenRow
                                          match currToken with
                                          | Parser.token.BIND -> let nextToken, nextTokenCol, nextTokenRow = getToken lexbuf
                                                               //   printfn "next token %A (%d, %d)" nextToken nextTokenCol nextTokenRow
                                                                 if guideCol() < nextTokenCol then
                                                                     pushToken (nextToken, nextTokenCol, nextTokenRow)
                                                                     pushToken (Parser.token.BLOCKBEGIN, nextTokenCol, nextTokenRow)
                                                                 else
                                                                     failwith "Indentation error"
                                          | Parser.token.EOF -> // printfn "Generating EOF %A" tokenStack
                                                                pushToken (Parser.token.EOF, 0, currTokenCol)
                                          | _ -> ()
                                          currToken, currTokenCol, currTokenRow

    let retToken = match currToken, currCol, currRow with
                   | Parser.token.BLOCKBEGIN, col, row -> pushGuide (col, row)
                                                          currToken
                   | Parser.token.EOF, _, _ -> popGuide() |> ignore
                                               match guideStack with
                                               | [] -> lexbuf.IsPastEndOfStream <- true
                                                       currToken
                                               | _ -> // printfn "EOF guide = %A" guideStack
                                                      lexbuf.IsPastEndOfStream <- false
                                                      pushToken (Parser.token.EOF, 0, currRow)
                                                      Parser.token.BLOCKEND
                   | _ -> match guideStack with
                          | (col, row) :: _ -> if col = guideCol() && row <> currRow then
                                                   popGuide() |> ignore
                                                   pushGuide (currCol, currRow)
                                                   pushToken (currToken, currCol, currRow)
                                                   Parser.token.BLOCKSEP
                                               elif currCol < guideCol() then
                                                   popGuide() |> ignore
                                                   pushToken (currToken, currCol, currRow)
                                                   Parser.token.BLOCKEND
                                               else
                                                   currToken
                          | _ -> currToken

    printfn ">>> token: %A" retToken
    retToken
 