module ImpParser

    open System
    open Eval

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"
    
    let spaces         = many whitespaceChar
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 = (p1 .>> spaces .>>. p2)
    let (.>*>) p1 p2  = (p1 .>> spaces .>> p2)
    let (>*>.) p1 p2  = (p1 >>. spaces >>. p2)

    let parenthesise p = (pchar '(' >*>. p .>*> pchar ')')
    let charApostrophe p = (pchar ''' >*>. p .>*> pchar ''')

    let charListToStr (chars: List<char>) = String.Concat(Array.ofList(chars))
    let pid = pchar '_' <|> pletter .>>. many(pchar '_' <|> palphanumeric) |>>
                fun (c, cs) -> c::cs |> charListToStr
        

    
    let unop op a = op >*>. a
    let binop op a b = a .>*> op .>*>. b

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CAtomParse, caref = createParserForwardedToRef<cExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add" 
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [ModParse; MulParse; DivParse; AtomParse]
    
    let NegParse = pchar '-' >>. pint32 |>> fun x -> Mul (N -1 , N x)
    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    let PVParse = pPointValue >*>. ParParse |>> PV <?> "PV"
    let VParse = pid |>> V <?> "V"
    let CTIParse = pCharToInt >*>. parenthesise CAtomParse |>> CharToInt <?> "charToInt"
    do aref := choice [NegParse; NParse; PVParse; CTIParse; ParParse; VParse]

    let AexpParse = TermParse 

   
    
    let CParse = pchar '\'' >>. anyChar .>> pchar '\'' |>> C <?> "C"
    let CVParse = pCharValue >*>. parenthesise TermParse |>> CV <?> "CV"
    let ITCParse = pIntToChar >*>. parenthesise TermParse |>> IntToChar <?> "intToChar"
    let TUParser = pToUpper >*>. parenthesise CAtomParse |>> ToUpper <?> "toUpper"
    let TLParser = pToLower >*>. parenthesise CAtomParse |>> ToLower <?> "toLower"
    do caref := choice [CParse; CVParse; ITCParse; TUParser; TLParser]
   
    let CexpParse = CAtomParse

    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"

(* These five types will move out of this file once you start working on the project *)
    type coord      = int * int
    type squareProg = Map<int, string>
    type boardProg  = {
            prog       : string;
            squares    : Map<int, squareProg>
            usedSquare : int
            center     : coord
    
            isInfinite : bool   // For pretty-printing purposes only
            ppSquare   : string // For pretty-printing purposes only
        }

    type word   = (char * int) list
    type square = Map<int, squareFun>

    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"

    type boardFun2 = coord -> StateMonad.Result<square option, StateMonad.Error>
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }

    let mkBoard (bp : boardProg) = failwith "not implemented"

