module internal Eval

    open StateMonad

    (* Code for testing *)

    let hello = ('H', 4) :: ('E', 1) :: ('L' , 1) :: ('L', 1) :: ('O', 1) :: []
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    let add a b =
        a >>= fun x ->
            (b >>= fun y ->
                ret(x + y))
          
    let sub a b =
        a >>= fun x ->
            (b >>= fun y ->
                ret(x - y))
            
    let mul a b =
        a >>= fun x ->
            (b >>= fun y ->
                ret(x * y))
          
    let div a b =
        a >>= fun x ->
            (b >>= fun y ->
            if y <> 0 then ret(x / y) else fail DivisionByZero)

    let modu a b =
        a >>= fun x ->
            (b >>= fun y ->
            if y <> 0 then ret(x % y) else fail DivisionByZero)
    
    let comp a b =
        a >>= fun x ->
            (b >>= fun y ->
                ret(x = y))
            
    let lessthan a b =
        a >>= fun x ->
            (b >>= fun y ->
                ret(x < y))
            
    let conj a b =      
        a >>= fun x ->      
            (b >>= fun y -> 
                ret(x && y)) 
    
        
    let vowels = ['a';'e';'i';'o';'u']
    let isVowel c = List.exists(fun x -> x = System.Char.ToLower(c)) vowels
        
    
    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

   
    let rec arithEval a : SM<int> =
        match a with
        |N n -> ret n
        |V v -> lookup v
        |WL -> wordLength
        |PV pv -> arithEval pv >>= pointValue
        |Add(a1, a2) -> add (arithEval a1) (arithEval a2)
        |Sub(a1, a2) -> sub (arithEval a1) (arithEval a2)
        |Mul(a1, a2) -> mul (arithEval a1) (arithEval a2)
        |Div(a1, a2) -> div (arithEval a1) (arithEval a2)
        |Mod(a1, a2) -> modu (arithEval a1) (arithEval a2)
        |CharToInt cti -> charEval cti >>= fun x -> ret((int) x)
        
            
    and charEval c : SM<char> =
        match c with
        |C c -> ret(c)
        |CV cv -> arithEval cv >>= characterValue
        |ToUpper tu -> charEval tu >>= fun x -> ret(System.Char.ToUpper(x))
        |ToLower tl -> charEval tl >>= fun x -> ret(System.Char.ToLower(x))
        |IntToChar itc -> arithEval itc >>= fun x -> ret((char) x)

    and boolEval b : SM<bool> =
        match b with
        |TT -> ret(true)
        |FF -> ret(false)
        
        |AEq (a1, a2) -> comp (arithEval a1) (arithEval a2)
        |ALt(a1, a2) -> lessthan (arithEval a1) (arithEval a2)
        
        |Not b -> boolEval b >>= fun x -> ret(not(x))
        |Conj(b1, b2) -> conj (boolEval b1) (boolEval b2)
        
        |IsDigit c -> charEval c >>= fun x -> ret(System.Char.IsDigit(x))
        |IsLetter c -> charEval c >>= fun x -> ret(System.Char.IsLetter(x))
        |IsVowel c -> charEval c >>= fun x -> ret(isVowel x)



    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm = failwith "Not implemented"


    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
    