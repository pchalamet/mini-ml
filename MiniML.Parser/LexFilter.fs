module MiniML.LexFilter
open FSharp.Text.Lexing
open MiniML
open MiniML.Lexer


type ParseException(message, position: Position) =
    inherit System.Exception($"Parsing error: {message} ({position.Column}, {position.Line})")

let startBlockMarkers = Set [ Parser.token.BIND
                              Parser.token.THEN
                              Parser.token.ELSE ]

type LexFilter() = class end
with
    let mutable prevToken = Parser.token.DUMMY
    let mutable delayedTokens = [ Parser.token.BLOCKBEGIN, Position.Empty ]
    let mutable blocks = [ Position.Empty ]
    let mutable isEOF = false

    member _.NextToken (lexbuf: LexBuffer<char>) =

        let getToken() =
            match delayedTokens with
            | (token, position) :: tail -> delayedTokens <- tail
                                           token, position
            | _ -> token lexbuf, lexbuf.StartPos

        let isPastEndOfStream = lexbuf.IsPastEndOfStream
        let nextToken, nextTokenStart = getToken()

        let unput replacementToken =
            delayedTokens <- (nextToken, nextTokenStart) :: delayedTokens
            lexbuf.IsPastEndOfStream <- isPastEndOfStream
            replacementToken

        let pushBlock position =
            blocks <- position :: blocks

        let popBlock () =
            match blocks with
            | head :: tail -> blocks <- tail
                              Some head
            | _ -> None

        isEOF <- nextToken = Parser.token.EOF
        let token = if isEOF then
                        match popBlock() with
                        | None -> Parser.token.EOF
                        | Some _ -> unput Parser.token.BLOCKEND
                    else
                        // Here we start a block (BIND, THEN ELSE...) - just ensure next token is after current block column
                        if startBlockMarkers |> Set.contains prevToken then
                            if nextTokenStart.Column <= blocks.Head.Column then ParseException("Indentation error", nextTokenStart) |> raise
                            blocks <- nextTokenStart :: blocks
                            unput Parser.token.BLOCKBEGIN
                        else
                            // We do not start a block here
                            // First check if we need to delimit statements (BLOCKSEP) - condition is same column but different lines
                            // otherwise it's just a new token
                            let block = blocks.Head

                            // example:
                            // let a = <BLOCKBEGIN> 42 <BLOCKEND> <BLOCKSEP>
                            // let b = <BLOCKBEGIN> 10 <BLOCKEND>
                            //
                            // let a =
                            //     <BLOCKBEGIN> 42 <BLOCKEND> <BLOCKSEP>
                            // let b =
                            //   <BLOCKBEGIN> 10 <BLOCKEND>
                            if nextTokenStart.Column < block.Column then
                                blocks <- blocks.Tail
                                if nextTokenStart.Column > blocks.Head.Column then ParseException("Indentation error", nextTokenStart) |> raise
                                unput Parser.token.BLOCKEND
                            elif nextTokenStart.Column = block.Column && nextTokenStart.Line <> block.Line then
                                blocks <- nextTokenStart :: blocks.Tail
                                unput Parser.token.BLOCKSEP
                            else
                                nextToken

        prevToken <- token
        printfn "Token = %A" token
        token
