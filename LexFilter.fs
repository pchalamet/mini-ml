module LexFilter
open FSharp.Text.Lexing

type LexFilter() = class end
with
    let mutable prevToken = Parser.token.DUMMY
    let mutable prevPosition = Position.Empty
    let mutable delayedTokens = [ Parser.token.BLOCKBEGIN, Position.Empty ]
    let mutable blockStack = [ Position.Empty ]
    let mutable isEOF = false

    member _.NextToken (lexbuf: LexBuffer<char>) =

        let getToken() =
            match delayedTokens with
            | (token, position) :: tail -> delayedTokens <- tail
                                           token, position
            | _ -> Lexer.token lexbuf, lexbuf.StartPos

        let isPastEndOfStream = lexbuf.IsPastEndOfStream
        let nextToken, nextTokenStart = getToken()

        let unput replacementToken =
            delayedTokens <- (nextToken, nextTokenStart) :: delayedTokens
            lexbuf.IsPastEndOfStream <- isPastEndOfStream
            replacementToken

        let pushBlock position =
            blockStack <- position :: blockStack

        let popBlock () =
            match blockStack with
            | head :: tail -> blockStack <- tail
                              Some head
            | _ -> None

        isEOF <- nextToken = Parser.token.EOF
        let token = if isEOF then
                        match popBlock() with
                        | None -> Parser.token.EOF
                        | Some _ -> unput Parser.token.BLOCKEND
                    else
                        let startBlockMarkers = Set [ Parser.token.BIND; Parser.token.THEN; Parser.token.ELSE]
                        if startBlockMarkers |> Set.contains prevToken then
                            if prevPosition.Column < nextTokenStart.Column then
                                blockStack <- nextTokenStart :: blockStack
                                unput Parser.token.BLOCKBEGIN
                            else
                                failwith "Indentation error"
                        else
                            let block = blockStack.Head
                            if block.Column = nextTokenStart.Column && block.Line <> nextTokenStart.Line then
                                blockStack <- nextTokenStart :: blockStack.Tail
                                unput Parser.token.BLOCKSEP
                            elif nextTokenStart.Column < block.Column then
                                blockStack <- blockStack.Tail
                                unput Parser.token.BLOCKEND
                            else
                                nextToken

        prevToken <- token
        printfn "Token = %A" token
        token
