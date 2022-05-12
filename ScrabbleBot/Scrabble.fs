namespace Wordfeud

    open System
    open MultiSet
    open Parser
    open ScrabbleUtil
    open ScrabbleUtil.Dictionary
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
            
        
        // fold over listen man får af CMSuccess - ms, som er moves - add to Map af board = boardsWithWords.
        let updateBoard (ms: ((int * int) * (uint32 * (char * int))) list) (st: state) : state=
            let piecesOnBoard = List.fold (fun acc ((x, y), (_, (letter, _))) -> Map.add (x, y) letter acc) st.boardWithWords ms
                // mkState - med alt i statet
            let st' = mkState st.board piecesOnBoard st.dict st.playerNumber st.hand
            st'
        // add playerPoints - ellers ligge vi det ikke til
        
            
        //step med givent bogstav og states dict
        // vi får et dict tilbage, som skal bruges
        // fold over hånd og gennem hånden indtil vi får et bogstav vi kan steppe med.
        // fjern bogstav fra hånd, opdateret hånd gives med videre til næste step i dict
        // Abort, hvis vi ikke kan ligge et ord
        // Prøve alle muligheder på hele hånden
        
     
        //let rec (b, d) = Dict.step c dict
        
       
    
    
    

    module Scrabble =
        open System.Threading
        
        type dir = Up | Down | Left | Right

        let next d (x,y)=
            match d with
            | Up -> (x, y-1)
            | Down -> (x,y+1)
            | Left  -> (x-1, y)
            | Right  -> (x+1,y)
    
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
   
                //stepper down -> used to find words to place vertically (hello nedad)
                (*let rec stepperDown hand dict (coord: coord) dir wordSoFar =
                    match Map.tryFind coord st.boardWithWords with
                    | Some c -> // tile is occupied  proceed from next coord
                        match Dictionary.step c dict with
                        | Some (true, dict') ->
                            if (Map.tryFind coord st.boardWithWords = Some c) then
                                stepperDown hand dict' (next coord dir) dir wordSoFar
                            else
                                MultiSet.fold (fun acc piece -> 
                                    match Map.find piece hand with
                                    | (id, (c, pv)) as tile ->
                                        let res = stepperDown (MultiSet.removeSingle piece hand) d (x, y-1) c (((x,y-1), tile)::wordSoFar)
                                        if List.length res > 0 then
                                            res
                                        else
                                            acc
                                ) [] hand
                                
                        | None -> []
                    | None -> []*)
                    
                    (*
                    if (checkLeft coord && checkRight coord) then
                        match Dictionary.step c dict with
                        | Some (true, d) ->
                            match Map.find pieces piece with
                            | tile -> wordSoFar
                           *)
                
                        
                (*
                let findWord (st: State.state) =
                    //starting with an empty board. This is the first move
                    if (Map.isEmpty st.boardWithWords) then        
                         let rec stepDownDict hand (dict: Dict) (dir: dir) (coord: coord) wordSoFar =
                             //look through your hand
                             MultiSet.fold (fun acc brik ->
                                 //find the char from the uint in your hand
                                 match Map.find brik pieces with
                                 | (id, (c, pv)) as tile ->
                                    //use the char to step in the dict
                                    match Dictionary.step c dict with
                                    | Some (b, d) ->
                                        //define recursive call with new hand and the tile to the right, add coord + tile to list
                                        let result = stepDownDict (MultiSet.removeSingle brik hand) d dir (next dir coord) ((coord, tile)::wordSoFar)
                                        //result has not yet been called here, how can it have a length?
                                        if List.length result > 0 then
                                        //is the method result called or do we return the list that is result?
                                        //should wordSoFar be returned at some point?
                                            result
                                        else
                                            acc
                                    | None -> []        
                             ) [] hand
                         stepDownDict st.hand st.dict Right (0,0) List.empty
                    *)
                    //else // stuff is on the board:
                        
                        
                        // findword from many coords and pick the best one
              
                //findWord st
                
                        // first move    
                // folde over hånden
                // finder char id i pieces
                        // finde ud af hvilken vej vi skal gå (høje)
                        // step i dict med char vi fandt
                        // fjerne char fra hånd
                        // matcher step option med some -> concat til wordsofar - ord listen
                        // hvis bool er true - ord er slut - smæk det bag på - kald inner (aux)
                        // if not - kalder funktionen - uden at proppe et ord med
                        // @ - add
                // word so far should look like the moves set
                let firstmove (st: State.state) (wordSoFar: List<(int * int) * (uint32 * (char * int))>) =
                        let rec aux = // dict, hand, board, movet man er i gang med at lave, som er listen, coordset
                        
                       
                // addToBoard: move +
                
                // når man har lavet successful play
                // 
                       
                        (*let knowAllCoords hand =
                           
                            Map.fold(fun acc (cd: coord) ->
                                
                                let checkAvailableUpOfWord ((x,y): coord) =
                                    match (Map.tryFind (x,y+1) st.boardWithWords) with
                                    |Some x -> false
                                    |None -> true
                                        
                                if checkAvailableUpOfWord cd then
                                    Map.fold (fun x chars -> 
                                        let writeWordFromAGivenCoord cd (h: MultiSet<uint32>) (dict: Dict) (c: char)=
                                            match Map.find cd st.boardWithWords with
                                            |c -> stepperDown h dict (Map.find c) pieces
                                               
                                            
                                            
                                        writeWordFromAGivenCoord cd hand dict
                                        ) st.boardWithWords
                                        
                                else
                                    let checkAvailableLeftOfWord ((x,y): coord) =
                                        match (Map.tryFind (x-1,y) st.boardWithWords) with
                                        |Some x -> false
                                        |None -> true
                                        
                                    if checkAvailableLeftOfWord cd then
                                        MultiSet.fold (fun x chars -> 
                                            let rec writeWordFromAGivenCoord cd (h: st.hand) (dict: Dict) =
                                                match Map.find cd with
                                                | c -> Dict.step c dict
                                                
                                            writeWordFromAGivenCoord snd (cd+1) updatedHand
                                                
                                            
                                            ) st.hand
                                            
                            ) st.boardWithWords*)

                        
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
                            (fun acc (coord, (tileNumber, (aChar, _))) -> Map.add coord aChar acc)
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
                            (fun acc (coord, (tileNumber, (aChar, _))) -> Map.add coord aChar acc)
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
                            (fun acc (coord, (tileNumber, (aChar, _))) -> Map.add coord aChar acc)
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
