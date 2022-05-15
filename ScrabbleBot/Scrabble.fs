﻿namespace Wordfeud

open System
open System.Collections.Generic
open MultiSet
open Parser
open ScrabbleUtil
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
        List.fold (fun acc (id, amount) -> MultiSet.add id amount acc) hand newPieces
        
    
    // fold over listen man får af CMSuccess - ms, som er moves - add to Map af board = boardsWithWords.
    let updateBoard (ms: ((int * int) * (uint32 * (char * int))) list) (st: state) : state=
        let piecesOnBoard = List.fold (fun acc ((x, y), (_, (letter, _))) -> Map.add (x, y) letter acc) st.boardWithWords ms
            // mkState - med alt i statet
        let st' = mkState st.board piecesOnBoard st.dict st.playerNumber st.hand
        st'
    // add playerPoints - ellers ligge vi det ikke til
    
   




module Scrabble =
    open System.Threading
    
    type dir = Up | Down | Left | Right

    let next d (x,y)=
        match d with
        | Up -> (x, y-1)
        | Down -> (x,y+1)
        | Left  -> (x-1, y)
        | Right  -> (x+1,y)
      
      // used for   
    let switchDir d =
        match d with
        | Down -> Up
        | Right -> Left
        | Left -> Right
        | Up -> Down
                
    let turnBetweenDownAndRight d =
        match d with
        | Down -> Right
        | Right -> Down
        | Left -> Up // not in use
        | Up -> Left // not in use
    let checkAvailableDirFromCoord cd dir (st: State.state) =
        match (Map.tryFind (next dir cd) st.boardWithWords) with
        |Some c -> false
        |None -> true
        
    let firstMove (counter: int) (st: State.state) (pieces: Map<uint32, 'a>) dir =
        let rec aux (dict: Dict) (hand: MultiSet.MultiSet<uint32>) (board: Map<coord, char>) (mov: ((int * int) * (uint32 * (char * int))) list) (coord: coord) =
            MultiSet.fold (fun acc piece _ ->
                // getting char and pv out of pieces
                let ch = Map.find piece pieces |> Seq.head |> fst // getting the char as it's the first in the set
                let pv = Map.find piece pieces |> Seq.head |> snd // pv second in the set: tile = Set<char*int>
                let cord = next Right coord
                
                let newHand = MultiSet.removeSingle piece hand // remove char from hand
               
                match Dictionary.step ch dict with // step med char
                | Some (b, d) ->
                    let letter = cord, (piece, (ch,pv))
                    let wordSofar = (mov@[letter])
                    
                    if b = true then
                        wordSofar::acc@(aux d newHand board wordSofar cord)
                    else
                        acc@(aux d newHand board wordSofar cord)
                        
                | None -> acc
                
            ) List.Empty hand
            
        aux st.dict st.hand st.boardWithWords List.empty (-1,0)
        
        (*
            anden metode, der folder over coord fra board, der kalder på generalMove
            
            FLY ->
 *)
    // helper method calling on generalMove
    // checke
 
            
    
    let generalMove (st: State.state) (pieces: Map<uint32, 'a>) (dir: dir) (coord: coord) =
   
        let rec aux (dict: Dict) (hand: MultiSet.MultiSet<uint32>) (board: Map<coord, char>) (mov: ((int * int) * (uint32 * (char * int))) list) (coord: coord) =
            
            MultiSet.fold (fun acc piece _ ->
               
                let ch = Map.find piece pieces |> Seq.head |> fst // getting the char as it's the first in the set
                let pv = Map.find piece pieces |> Seq.head |> snd // pv second in the set: tile = Set<char*int>
                //let cord = next dir coord
                
                match Map.tryFind coord st.boardWithWords with
                | Some c -> // something is on the board
                    match Dictionary.step c dict with
                    | Some (b',d') ->
                        aux d' hand board mov (next dir coord) // call with new found char, but don't add to WordSoFar list.
                    | None -> acc
                | None -> 
                    let newHand = MultiSet.removeSingle piece hand // remove char from hand
                   
                    match Dictionary.step ch dict with // step med char
                    | Some (b, d) ->
                        let letter = coord, (piece, (ch,pv))
                        let wordSoFar = (mov@[letter])
                               
                        if b = true then
                            wordSoFar::acc@(aux  d newHand board wordSoFar (next dir coord))
                        else
                            acc@(aux  d newHand board wordSoFar (next dir coord))    
                    | None -> acc
            ) List.Empty hand
//        if (dire = Right) then
//            let turn = turnBetweenDownAndRight dire
//            aux turn st.dict st.hand st.boardWithWords List.empty coord
//        else
//            let turn = dire
//            aux turn st.dict st.hand st.boardWithWords List.empty coord
       
        aux st.dict st.hand st.boardWithWords List.empty coord
        
    
    let longestWord (words: ((int * int) * (uint32 * (char * int))) list list) =
        List.fold(fun bestWord word ->
            if List.length bestWord > List.length word then
                bestWord
            else
                word
                
        ) List.Empty words
        
        // metode, der finder til ventre
        // matcher på board i en bestemt retning
        // hvis man finder, opdatere coord
        // returnere coordsæt
    let findMostDirectionalMove (st: State.state) (pieces: Map<uint32, 'a>) dir (coord: coord) =
        Map.fold(fun acc cord _ ->
            match Map.tryFind coord st.boardWithWords with
            | Some c -> next dir cord
            | None -> acc
        )coord st.boardWithWords
        
    let mapMove (counter: int) (st: State.state) (pieces: Map<uint32, 'a>) (board: Map<coord, char>) dir (coord: coord) =
        // check for words one way (Right or Down)
        let coordsToStartWith = findMostDirectionalMove st pieces (switchDir dir) coord
        //let moveOneDirection =
        generalMove st pieces dir coordsToStartWith
        
        // check for words other way (Down or Right)
        // if we do this, it might try to place "FANNEDOUTLAY", so it tries to places outlay after the d - fanned, which was first move
        
        
        (*let coordsToStartWithSnd = findMostDirectionalMove st pieces dir coord
        let moveAnotherDirection = generalMove st pieces dir coordsToStartWithSnd
        let moveList = moveOneDirection@moveAnotherDirection
        moveList*)
        
    let mutable i = 1
    let X() =
        i <- i + 1
        i
            
    let playGame cstream (pieces: Map<uint32, tile>) (st: State.state) =

        let rec aux (st: State.state) =
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint
                "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"

            let input = System.Console.ReadLine()
            let move = RegEx.parseMove input

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            //send cstream (SMPlay move)

            let tilesToChange = MultiSet.toList st.hand
            
            
            
            let findMove =
               
                if (Map.isEmpty st.boardWithWords) then
                    firstMove (X()) st pieces Right
                    
                else
                    if (i % 2 = 0) then
                        mapMove (X()) st pieces st.boardWithWords Down (0,0)
                        
                    else
                        mapMove (X()) st pieces st.boardWithWords Right (0,0)
                        
            
            let wordMove = longestWord findMove  
            let playMove =
                match findMove with
                |[] -> send cstream (SMChange tilesToChange)
                |_ -> send cstream (SMPlay wordMove)
           
            playMove
            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess (moves, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                debugPrint (sprintf "----------You made a successful move! New tiles added to hand---------")

                let removedFromHand = State.removeFromHand st.hand moves

                let addedToHand =
                    State.addToHand removedFromHand newPieces

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
            | RCM (CMChange(playerId, numberOfTiles)) ->
                // Other player successfully changed pieces
                // st.hand
                let emptyHand = MultiSet.empty
                
                let st' =
                    State.mkState st.board st.boardWithWords st.dict st.playerNumber emptyHand
                aux st'    
            | RCM (CMChangeSuccess(newTiles)) ->
                // we have successfully changed pieces
                let emptyHand = MultiSet.empty
                let newHand = State.addToHand emptyHand newTiles    
                let st' =
                    State.mkState st.board st.boardWithWords st.dict st.playerNumber newHand
                aux st'    
            | RCM (CMGameOver _) -> ()

            | RCM a -> failwith (sprintf "not implmented: %A" a)

            | RGPE err ->
                printfn "Gameplay Error:\n%A" err
                aux st


        aux st

    let startGame 
            (boardP : boardProg)
            (dictf : bool -> Dictionary.Dict)  
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
    
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

    //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board Map.empty dict playerNumber handSet)
