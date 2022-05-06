namespace Wordfeud

    open System
    open MultiSet
    open Parser
    open ScrabbleUtil
    open ScrabbleUtil.ServerCommunication

    open System.IO

    open ScrabbleUtil.DebugPrint
    open StateMonad

    // The RegEx module is only used to parse human input. It is not used for the final product.

    module RegEx =
        open System.Text.RegularExpressions

        let (|Regex|_|) pattern input =
            let m = Regex.Match(input, pattern)

            if m.Success then
                Some(List.tail [ for g in m.Groups -> g.Value ])
            else
                None

        let parseMove ts =
            let pattern =
                @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?"

            Regex.Matches(ts, pattern)
            |> Seq.cast<Match>
            |> Seq.map
                (fun t ->
                    match t.Value with
                    | Regex pattern [ x; y; id; c; p ] -> ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                    | _ -> failwith "Failed (should never happen)")
            |> Seq.toList

    module Print =

        let printHand pieces hand =
            hand
            |> MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

    module State =
        // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
        // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
        // but it could, potentially, keep track of other useful
        // information, such as number of players, player turn, etc.

        type state =
            { board: Parser.board
              boardWithWords: Map<coord, char>
              dict: ScrabbleUtil.Dictionary.Dict
              playerNumber: uint32
              hand: MultiSet.MultiSet<uint32> }

        let mkState b bww d pn h =
            { board = b
              boardWithWords = bww
              dict = d
              playerNumber = pn
              hand = h }

        let board st = st.board
        let boardWithWords st = st.boardWithWords
        let dict st = st.dict
        let playerNumber st = st.playerNumber
        let hand st = st.hand

        let removeFromHand hand moves =
            List.fold (fun acc piece -> MultiSet.removeSingle (fst (snd (piece))) acc) hand moves

        let addToHand hand newPieces =
            List.fold (fun acc (x, k) -> MultiSet.add x k acc) hand newPieces
            
        let getCharFromCoords hand pieces =
            hand |>
            MultiSet.fold (fun _ x -> Map.find x pieces)

    module Scrabble =
        open System.Threading

        let playGame cstream pieces (st: State.state) =

            let rec aux (st: State.state) =
                Print.printHand pieces (State.hand st)

                // remove the force print when you move on from manual input (or when you have learnt the format)
                forcePrint
                    "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"

                let input = System.Console.ReadLine()
                let move = RegEx.parseMove input

                debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                send cstream (SMPlay move)

                let msg = recv cstream
                debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

                //find out what is on the board, look through coords down and right
                
                let lookThroughCoords =
                    Map.fold (fun acc (spaceImLookingAt: coord) ->
                        // CASE 1: 
                        // is there something over?
                        // if not, find ud af hvilket bogstav vi står ved
                        // then, step med givent bogstav og states dict
                            // vi får et dict tilbage, som skal bruges
                            // fold over hånd og gennem hånden indtil vi får et bogstav vi kan steppe med.
                            // fjern bogstav fra hånd, opdateret hånd gives med videre til næste step i dict
                            // Abort, hvis vi ikke kan ligge et ord
                            // Prøve alle muligheder på hele hånden
                            
                        // SECOND CASE: DER LIGGER NOGET OVER BOGSTAVET
                            // 2 muligheder:
                                // 1. mulighed: kigge til venstre
                                    // hvis ventre er tom: så følg case one
                                // 2. mulighed: der er noget til venstre
                                    // "backtracke" til vi finder et tomt space
                                    // herefter steppe igennem trien med de givne bogstaver den finder
                                    // se om den kan færdiggøre eller videre skrive på ordet
                                    // første og bedste ord den finder
                        // hver eneste gang der ligges et bogstav, så skal man hele tiden holde øje med om der
                            // ligger noget på felter
                            // hvis der gør, skal der steppes ligesom CASE 2 2. mulighed
                            // hvis vi ikke kan færdiggøre et ord, find et andet sted og prøve at ligge et ord
                            
                        // GENERAL NOTE: lav hjælpe metoder
                            // hvor de skal have parameterne: coord, dict, word, hånd
                            // DE SKAL VÆRE REC, og hvor parameterne opdateres
                            // fx. skal hånd opdateres til hand - letter, hvis vi kan ligge noget/ laver et sucessful step
                            // Abort, hvis vi ikke kan.
                            
                            (*let checkAvailableToTheLeftOfWord ((x,y): coord) =
                                if (Map.tryFind (x-1,y) st.boardWithWords) then false
                                else true
 
                            let checkIfAvailableToTheRightOfWord ((x,y): coord) =
                                if (Map.tryFind (x+1,y) st.boardWithWords) then false
                                else true
                                
                            if(checkAvailableToTheLeftOfWord && checkIfAvailableToTheRightOfWord) then 
                                let rec lookRight ((x,y): coord) (h: State.hand) (d: Dict) =
                                    if(Dict.step (State.getCharFromCoords h pieces) d) then Dict.step
                                    
                                    
                            let checkAvailableUpOfWord ((x,y): coord) =
                                if (Map.tryFind (x,y+1) st.boardWithWords) then false
                                else true
                            let checkAvailableDownOfWord ((x,y): coord) =
                                if (Map.tryFind (x,y-1) st.boardWithWords) then false
                                else true
                                
                            if(checkAvailableUpOfWord && checkAvailableDownOfWord) then                                
                                let rec lookDown ((x,y): coord) (isOccupied: bool) =
                                    if (Map.tryFind (x,y) st.boardWithWords) then lookDown (x, y+1) false
                                    else isFree = true*)
                                
                        ) st.boardWithWords
                
                //find out what words you can make in combination with the board
                //the board is infinite, so you always take a tile on the board as your first letter
                
                
                match msg with
                | RCM (CMPlaySuccess (moves, points, newPieces)) ->
                    (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                    debugPrint (sprintf "----------You made a successful move! New tiles added to hand---------")

                    let removedFromHand = State.removeFromHand st.hand moves

                    let addedToHand =
                        State.addToHand removedFromHand newPieces

                    // løbe igennem alle bogstaver der er lagt
                    // hvordan får vi infomationer om tilen til uint - hvordan ved vi hvilket bogstav det er
                    // 1. hold styr på bogstaver

                    let boardWithNewWordAdded =
                        List.fold
                            (fun acc (coord, (tileNumber, (_, _))) -> Map.add coord tileNumber acc)
                            st.boardWithWords
                            moves

                    let st' =
                        State.mkState st.board boardWithNewWordAdded st.dict st.playerNumber addedToHand

                    aux st' 

                | RCM (CMPlayed (pid, moves, points)) ->
                    (* Successful play by other player. Update your state *)
                    debugPrint (sprintf "----------Player %d made a successful move!---------" (State.playerNumber st))

                    let boardWithNewWordAdded =
                        List.fold
                            (fun acc (coord, (tileNumber, (_, _))) -> Map.add coord tileNumber acc)
                            st.boardWithWords
                            moves

                    let st' =
                        State.mkState st.board boardWithNewWordAdded st.dict st.playerNumber st.hand

                    aux st'

                | RCM (CMPlayFailed (pid, moves)) ->
                    (* Failed play. Update your state *)
                    debugPrint (sprintf "----------Player %d made a failed a move!---------" (State.playerNumber st))

                    let boardWithNewWordAdded =
                        List.fold
                            (fun acc (coord, (tileNumber, (_, _))) -> Map.add coord tileNumber acc)
                            st.boardWithWords
                            moves

                    let st' =
                        State.mkState st.board boardWithNewWordAdded st.dict st.playerNumber st.hand

                    aux st'

                | RCM (CMGameOver _) -> ()

                | RCM a -> failwith (sprintf "not implmented: %A" a)

                | RGPE err ->
                    printfn "Gameplay Error:\n%A" err
                    aux st


            aux st

        let startGame
            (boardP: boardProg)
            (dictf: bool -> Dictionary.Dict)
            (numPlayers: uint32)
            (playerNumber: uint32)
            (playerTurn: uint32)
            (hand: (uint32 * uint32) list)
            (tiles: Map<uint32, tile>)
            (timeout: uint32 option)
            (cstream: Stream)
            =
            debugPrint (
                sprintf
                    "Starting game!
                          number of players = %d
                          player id = %d
                          player turn = %d
                          hand =  %A
                          timeout = %A\n\n"
                    numPlayers
                    playerNumber
                    playerTurn
                    hand
                    timeout
            )

            //let dict = dictf true // Uncomment if using a gaddag for your dictionary
            let dict = dictf false // Uncomment if using a trie for your dictionary
            let board = Parser.mkBoard boardP

            let handSet =
                List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

            fun () -> playGame cstream tiles (State.mkState board Map.empty dict playerNumber handSet)
