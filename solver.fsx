#r "nuget:fparsec"
open FParsec

open System
type Color =
    | Black
    | White
    | Red
    | Yellow
    | Blue
    static member ofChar c =
        match c with
        | 'K'|'k' -> Black
        | 'W'|'w' -> White
        | 'R'|'r' -> Red
        | 'Y'|'y' -> Yellow
        | 'B'|'b' -> Blue
        | _ -> failwith "Wrong color char."
    static member ofString (str: string) =
        Color.ofChar str[0]

type SimonColor =
    | SRed
    | SBlue
    | SGreen
    | SYellow
    static member toInt c =
        match c with
        | SRed -> 0
        | SBlue -> 1
        | SGreen -> 2
        | SYellow -> 3
    static member toString c =
        match c with
        | SRed -> "Red"
        | SBlue -> "Blue"
        | SGreen -> "Green"
        | SYellow -> "Yellow"

let ( =-= ) expected actual =
    String.Equals(expected, actual, StringComparison.InvariantCultureIgnoreCase)   



let pBlack = stringCIReturn "Black" Black <|> stringCIReturn "K" Black
let pWhite = stringCIReturn "White" White <|> stringCIReturn "W" White
let pRed = stringCIReturn "Red" Color.Red <|> stringCIReturn "R" Color.Red
let pYellow = stringCIReturn "Yellow" Color.Yellow <|> stringCIReturn "Y" Color.Yellow
let pBlue = stringCIReturn "Blue" Color.Blue <|> stringCIReturn "B" Color.Blue

let pSRed = stringCIReturn "Red" SRed <|> stringCIReturn "R" SRed
let pSYellow = stringCIReturn "Yellow" SYellow <|> stringCIReturn "Y" SYellow
let pSBlue = stringCIReturn "Blue" SBlue <|> stringCIReturn "B" SBlue
let pSGreen = stringCIReturn "Green" SGreen <|> stringCIReturn "G" SGreen

let pColor = pBlack <|> pWhite <|> pRed <|> pYellow <|> pBlue
let pColors = many (pColor .>> spaces)

let pSimonColor = pSRed <|> pSBlue <|> pSGreen <|> pSYellow
let pSimonColors = many (pSimonColor .>> spaces)

let pYes = stringCIReturn "Yes" true <|> stringCIReturn "Y" true

let pNo = stringCIReturn "No" false <|> stringCIReturn "N" false

let pYN = pYes <|> pNo

let pQ = stringCIReturn "Q" 'Ϙ'
let pCo = stringCIReturn "Co" '©'
let p6  = stringCIReturn "6" 'б'
let pPsi= stringCIReturn "Psi" 'Ψ'
let pAt = stringCIReturn "At" 'Ѧ'
let pW  = stringCIReturn "W" 'Ѽ'
let pP  = stringCIReturn "P" '¶'
let pSm = stringCIReturn "Sm" 'ټ'
let pE  = stringCIReturn "E" 'Ӭ'
let pY  = stringCIReturn "Y" 'ƛ'
let pC  = stringCIReturn "C" 'Ͼ'
let pCe = stringCIReturn "Ce" 'Ҩ'
let pBt = stringCIReturn "Bt" 'Ѣ'
let pPP = stringCIReturn "++" '҂'
let pZ  = stringCIReturn "Z" 'Ϟ'
let pKk = stringCIReturn "Kk" 'Җ'
let pSp = stringCIReturn "Sp" 'Ѭ'
let pAe = stringCIReturn "Ae" 'æ'
let pSt = stringCIReturn "St" '★'
let pR  = stringCIReturn "R" 'Ԇ'
let pH  = stringCIReturn "H" 'ϗ'
let pQM = stringCIReturn "?" '¿'
let p3  = stringCIReturn "3" 'Ѯ'
let pN  = stringCIReturn "N" 'Ҋ'
let pOm = stringCIReturn "Om" 'Ω'

let pSymbol = pPsi <|> pCo <|> pAt <|> pSm <|> pCe <|> pBt <|> pPP <|> pKk <|> pSp <|> pAe <|> pSt <|> pQM <|> pOm <|> pQ <|> p6 <|> pW <|> pP <|> pE <|> pY <|> pC <|> pZ <|> pR <|> pH <|> p3 <|> pN

let pSymbols = many (pSymbol .>> spaces)

run pSymbols "W r Y kK"

let oneColorIn _ =
    match (run pColors (Console.ReadLine ())) with
                            | Failure (e,_,_) -> failwith $"Incorrect input : {e}"
                            | Success (r,_,_) -> r[0]

let getYNIn _ =
    match (run pYN (Console.ReadLine ())) with
                            | Failure (e,_,_) -> failwith $"Incorrect input : {e}"
                            | Success (r,_,_) -> r

let lastIndexOf value array =
    fst (Array.fold (fun (l,i) c -> if c=value then i,i+1 else l,i+1 ) (-1,0) array)

let numberOf key map =
    Map.tryFind key map |> Option.defaultValue 0

let mutable (lastSerialNumberDigit:int option) = None

let mutable (voyelInSerialNumber:bool option) = None

let mutable (strikes:int option) = None

let mutable (batteries:int option) = None

let mutable (litCAR:bool option) = None

let mutable (litFRK:bool option) = None

let newBomb _ =
    lastSerialNumberDigit <- None
    voyelInSerialNumber <- None
    strikes <- None
    batteries <- None
    litCAR <- None
    litFRK <- None

let getLastSerialNumberDigit _ =
    match lastSerialNumberDigit with
    | None ->   Console.Write "What's the last digit of serial number? : "
                lastSerialNumberDigit <- Some (int (Console.ReadLine()))
                Option.get lastSerialNumberDigit
    | s -> Option.get s

let getVoyelInSerialNumber _ =
    match voyelInSerialNumber with
    | None ->   Console.Write "Is there a voyel in the serial number? : "
                voyelInSerialNumber <- Option.op_Implicit (getYNIn ())
                Option.get voyelInSerialNumber
    | v -> Option.get v

let getStrikes _ =
    match strikes with
    | None ->   Console.Write "How many strikes did you do? : "
                strikes <- Some (int (Console.ReadLine()))
                Option.get strikes
    | s -> Option.get s


let getBatteries _ =
    match batteries with
    | None ->   Console.Write "How many batteries are there on the bomb? : "
                batteries <- Some (int (Console.ReadLine()))
                Option.get batteries
    | b -> Option.get b

let getLitCAR _ =
    match litCAR with
    | None ->   Console.Write "Is there a lit indicator with the label CAR? : "
                litCAR <- Option.op_Implicit (getYNIn ())
                Option.get litCAR
    | c -> Option.get c

let getLitFRK _ =
    match litFRK with
    | None ->   Console.Write "Is there a lit indicator with the label FRK? : "
                litFRK <- Option.op_Implicit (getYNIn ())
                Option.get litFRK
    | f -> Option.get f

let threeWires colors _ =
    let numberOfColors = 
        colors
        |> Array.countBy id
        |> Map.ofArray
    if numberOf Color.Red numberOfColors=0 then
        2
    elif colors[2]=White then
        3
    elif numberOf Blue numberOfColors>1 then
        lastIndexOf Blue colors + 1
    else
        3

let fourWires colors lastSNDigit =
    let numberOfColors = 
        colors
        |> Array.countBy id
        |> Map.ofArray
    if numberOf Red numberOfColors>1 && lastSNDigit&&&1 = 1 then
        lastIndexOf Red colors + 1
    elif (colors[3]=Yellow && numberOf Red numberOfColors = 0) || (numberOf Blue numberOfColors = 1) then
        1
    elif numberOf Yellow numberOfColors > 1 then
        4
    else
        2

let fiveWires colors lastSNDigit =
    let numberOfColors = 
        colors
        |> Array.countBy id
        |> Map.ofArray
    if colors[4] = Black && lastSNDigit&&&1 = 1 then
        4
    elif numberOf Red numberOfColors = 1 && numberOf Yellow numberOfColors > 1 then
        1
    elif numberOf Black numberOfColors = 0 then
        2
    else
        1

let sixWires colors lastSNDigit  =
    let numberOfColors = 
        colors
        |> Array.countBy id
        |> Map.ofArray
    if numberOf Yellow numberOfColors = 0 && lastSNDigit&&&1 = 1 then
        3
    elif numberOf Yellow numberOfColors = 1 && numberOf White numberOfColors > 1 then
        4
    elif numberOf Red numberOfColors = 0 then
        6
    else
        4

let solveWires _ = 
    Console.Write "What are the wire colors? : "
    let colors = Console.ReadLine()
    let colorCodes = match (run pColors colors) with
                        | Failure (e,_,_) -> failwith $"Incorrect input : {e}"
                        | Success (r,_,_) -> r |> List.toArray
    let functionToUse = match colorCodes.Length with
                        | 3 -> threeWires colorCodes
                        | 4 -> fourWires colorCodes
                        | 5 -> fiveWires colorCodes
                        | 6 -> sixWires colorCodes
                        | _ -> failwith "Bad number of wires."
    match lastSerialNumberDigit with
    | None ->   let ifEven = functionToUse 0
                let ifOdd = functionToUse 1
                if ifEven<>ifOdd then
                    printfn $"If the last digit of serial nuber is even cut wire {ifEven}, else, cut wire {ifOdd}."
                    functionToUse (getLastSerialNumberDigit ())
                else
                    ifEven
    | Some n ->   functionToUse n

let solveButton _ =
    Console.Write "What's the color of the button? : "
    let color = oneColorIn ()
    Console.Write "What's written on the button? : "
    let text = Console.ReadLine ()
    let held =
        if color = Blue && (text =-= "Abort" || text =-= "Annuler") then
            printfn "Press and hold the button."
            true
        elif getBatteries () > 1 && (text =-= "Detonate" || text =-= "Exploser") then
            printfn "Press the button and immediatly release it"
            false
        elif color = White && getLitCAR () then
            printfn "Press and hold the button."
            true
        elif getBatteries () > 2 && getLitFRK () then
            printfn "Press the button and immediatly release it"
            false
        elif color = Yellow then
            printfn "Press and hold the button."
            true
        elif color = Red && (text =-= "Hold" || text =-= "Maintenir") then
            printfn "Press the button and immediatly release it"
            false
        else
            printfn "Press and hold the button."
            true
    if held then
        Console.Write "After few seconds, a strip will light up. What is its color? : "
        let stripcolor = oneColorIn ()
        match stripcolor with
        | Blue -> printfn "Release when there's a 4 somewhere on the timer."
        | Yellow -> printfn "Release when there's a 5 somewhere on the timer."
        | _ -> printfn "Release when there's a 1 somewhere on the timer."

let containsAll array elements =
    Array.fold(fun s e -> s && Array.contains e array) true elements

let getSymbols _ = 
    let text = Console.ReadLine ()
    let parsed = run pSymbols text
    match parsed with
    | Failure (e,_,_) -> failwith $"Bad input : {e}"
    | Success (r,_,_) -> Array.ofList r

let solveKeypad _ =
    let columns = [|[|'Ϙ'; 'Ѧ'; 'ƛ'; 'Ϟ'; 'Ѭ'; 'ϗ'; 'Ͼ'|]
                    [|'Ӭ'; 'Ϙ'; 'Ͼ'; 'Ҩ'; '★'; 'ϗ'; '¿'|]
                    [|'©'; 'Ѽ'; 'Ҩ'; 'Җ'; 'Ԇ'; 'ƛ'; '★'|]
                    [|'б'; '¶'; 'Ѣ'; 'Ѭ'; 'Җ'; '¿'; 'ټ'|]
                    [|'Ψ'; 'ټ'; 'Ѣ'; 'Ͼ'; '¶'; 'Ѯ'; '★'|]
                    [|'б'; 'Ӭ'; '҂'; 'æ'; 'Ψ'; 'Ҋ'; 'Ω'|]|]
    printfn """Symbol names :
Ϙ:Q        ©:Co     б:6     Ψ:Psi      Ѧ:At     Ѽ:W
¶:P        ټ:Sm     Ӭ:E     ƛ:Y        Ͽ/Ͼ:C    Ҩ:Ce
Ѣ:Bt       ҂:++     Ϟ:Z     Җ:Kk       Ѭ:Sp     æ:Ae
☆/★:St    Ԇ:R      ϗ:H     ¿:?        Ѯ:3      Ҋ:N   
Ω:Om"""
    Console.Write $"What are all the symbols (use the names over)? : "
    let symbols = getSymbols()
    let mutable col = columns[0]
    for c in columns do
        if containsAll c symbols then
            col <- c
    let inOrder = Array.sortBy(fun e -> Array.IndexOf (col,e) ) symbols
    printfn $"Press buttons in this order : {inOrder[0]} {inOrder[1]} {inOrder[2]} {inOrder[3]}"






let simonWithVoyel = [|[|SBlue;SRed;SYellow;SGreen|];[|SYellow;SGreen;SBlue;SRed|];[|SGreen;SRed;SYellow;SBlue|]|]

let simonWithoutVoyel = [|[|SBlue;SYellow;SGreen;SRed|];[|SRed;SBlue;SYellow;SGreen|];[|SYellow;SGreen;SBlue;SRed|]|]

let rec solveSimon _ =
    let voyel = getVoyelInSerialNumber ()
    let table = if voyel then
                    simonWithVoyel
                else 
                    simonWithoutVoyel
    let moves = table[0]
    Console.Write "What are the flashing colors? : "
    let text = Console.ReadLine ()
    
    if text =-= "D" then
        ()
    else
        let colorsIn = 
            match (run pSimonColors (text)) with
            | Failure (e,_,_) -> failwith $"Bad input : {e}"
            | Success (r,_,_) -> r
        printf "Press these colors : "
        for c in colorsIn do
            printf $"{SimonColor.toString moves[SimonColor.toInt c]} "
        printfn ""
        solveSimon ()

let rec askNumberMemory (m:string) =
    Console.Write m
    let input=int (Console.ReadLine ())
    if input > 4 || input < 1 then
        printfn "nope"
        askNumberMemory m
    else
        input

let AskMemoryWithLabel l =
    let p = askNumberMemory $"Press the button with label {l} and tell its position : "
    p,l

let AskMemoryWithPosition p =
    let l = askNumberMemory $"Press the button in position {p} and tell its label : "
    p,l

let solveMemory _ =
    let stage1p,stage1l =
        match askNumberMemory "What is the number on the screen ? : " with
        | 1|2 -> AskMemoryWithPosition 2
        | 3 -> AskMemoryWithPosition 3
        | _ -> AskMemoryWithPosition 4
    let stage2p,stage2l =
        match askNumberMemory "Now, what is the number on the screen ? : " with
        | 1 -> AskMemoryWithLabel 4
        | 3 -> AskMemoryWithPosition 1
        | _ -> AskMemoryWithPosition stage1p
    let _,stage3l =
        match askNumberMemory "Now, what is the number on the screen ? : " with
        | 1 -> AskMemoryWithLabel stage2l
        | 2 -> AskMemoryWithLabel stage1l
        | 3 -> AskMemoryWithPosition 3
        | _ -> AskMemoryWithLabel 4
    let stage4p,stage4l =
        match askNumberMemory "Now, what is the number on the screen ? : " with
        | 1 -> AskMemoryWithPosition stage1p
        | 2 -> AskMemoryWithPosition 1
        | _ -> AskMemoryWithPosition stage2p
    let finall =
        match askNumberMemory "Now, what is the number on the screen ? : " with
        | 1 -> stage1l
        | 2 -> stage2l
        | 3 -> stage4l
        | _ -> stage3l
    printfn $"Finally, press the button with label {finall}"  


