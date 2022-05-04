module internal Parser

    
    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open System
    open Eval
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
    
    
 (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        usedSquare    : square
        squares       : boardFun
    }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); usedSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
