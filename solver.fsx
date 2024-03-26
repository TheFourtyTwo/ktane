#r "nuget:fparsec"
#r "nuget: Spectre.Console, 0.48.1-preview.0.36 "
open FParsec
open Spectre.Console


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

    static member ConsoleColor c =
        match c with
        | Black -> "black"
        | White -> "white"
        | Red -> "red"
        | Yellow -> "yellow"
        | Blue -> "blue"


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
    override this.ToString() =
        match this with
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

let oneColorIn input =
    match (run pColors input) with
                            | Failure (e,_,_) -> failwith $"Incorrect input : {e}"
                            | Success (r,_,_) -> r[0]


let lastIndexOf value array =
    fst (Array.fold (fun (l,i) c -> if c=value then i,i+1 else l,i+1 ) (-1,0) array)

let numberOf key map =
    Map.tryFind key map |> Option.defaultValue 0

type Bomb =
    { LastSerialNumberDigit: int option 
      VoyelInSerialNumber: bool option
      Strikes: int option
      Batteries: int option
      LitCAR: bool option
      LitFRK: bool option
    }
    static member empty =
        { LastSerialNumberDigit = None
          VoyelInSerialNumber = None
          Strikes = None
          Batteries = None
          LitCAR = None
          LitFRK = None}

type Defuser<'a> = Bomb -> Bomb * 'a

module Defuser =
    let ret x : Defuser<_> = fun bomb -> bomb, x
    let bind (f : 'a -> Defuser<'b>) (x: Defuser<'a>) : Defuser<'b> = 
        fun bomb ->
             let b,a = x bomb
             f a b

    let lift (f: 'a -> 'b) : 'a -> Defuser<'b> = fun a bomb -> bomb, f a 

    let combine (x: Defuser<unit>) (y: Defuser<'a>) : Defuser<'a> =
        fun bomb ->
            let b,_ = x bomb
            y b

    let delay (x: Defuser<'a>) = fun() -> x 
    let map (f: 'a -> 'b) (x: Defuser<'a>) : Defuser<'b> =
        fun bomb ->
            let bomb,v = x bomb
            bomb, f v

let (<&&>) (x: Defuser<bool>) (y: Defuser<bool>) : Defuser<bool> =
    fun bomb ->
        let bomb, bx = x bomb 
        if not bx then
            bomb, false
        else
            y bomb

let (<||>) (x: Defuser<bool>) (y: Defuser<bool>) : Defuser<bool> =
    fun bomb ->
        let bomb, bx = x bomb 
        if bx then
            bomb, true
        else
            y bomb


let (~~) x = Defuser.ret x
let (>>=) x f = Defuser.bind f x
let (<!>) f x = Defuser.map f x

type DefuserBuilder() =
    member _.Bind(x,f) = Defuser.bind f x
    member _.Return(x) = Defuser.ret x
    member _.BindReturn(x,f) = Defuser.map f x
    member _.ReturnFrom(x) = x
    member _.Zero() = Defuser.ret ()
    member _.Combine(x, y) = Defuser.combine x y
    member _.For(items: seq<'a>, body: 'a -> Defuser<unit>) : Defuser<unit> = 
        fun bomb -> 
            ((bomb,items) ||> Seq.fold (fun bomb item -> body item bomb |> fst )),()

    member _.Delay(f)  = fun bomb -> f () bomb


let defuser = DefuserBuilder()

let getBomb (f: Bomb -> 'a) : Defuser<'a> =
    fun bomb -> bomb, f bomb

let updateBomb (f: Bomb -> Bomb) : Defuser<unit> =
    fun bomb -> f bomb,()

let lastSNDigit () =
    defuser {
        match! getBomb _.LastSerialNumberDigit with
        | None ->
            let input = AnsiConsole.Ask<int> "What's the last digit of serial number? "
            do! updateBomb (fun bomb -> { bomb with LastSerialNumberDigit = Some input })
            return input
        | Some s -> return s
    }

let hasVoyelInSN() =
    defuser {
        match! getBomb _.VoyelInSerialNumber with
        | None ->   
            let input = AnsiConsole.Confirm "Is there a voyel in the serial number? : "
            do! updateBomb (fun bomb -> { bomb with VoyelInSerialNumber = Some input })
            return input
        | Some v -> return v
    }

let getStrikes() =
    defuser {
        match! getBomb _.Strikes with
        | None ->
            let strikes = AnsiConsole.Ask<int> "How many strikes did you do? "
            do! updateBomb (fun bomb -> { bomb with Strikes = Some strikes })
            return strikes
        | Some strikes -> return strikes
    }


let getBatteries() =
    defuser {
        match! getBomb _.Batteries with
        | None ->   
            let batteries = AnsiConsole.Ask<int> "How many batteries are there on the bomb? "
            do! updateBomb (fun bomb -> { bomb with Batteries = Some batteries })
            return batteries
        | Some batteries -> return batteries
    }

let getLitCAR()  =
    defuser {
        match! getBomb _.LitCAR with
        | None ->   
            let litCAR = AnsiConsole.Confirm("Is there a lit indicator with the label CAR? ", false)
            do! updateBomb (fun bomb -> { bomb with LitCAR = Some litCAR})
            return litCAR
        | Some c -> return c
    }

let getLitFRK() =
    defuser {
        match! getBomb _.LitFRK with
        | None ->
            let litFRK = AnsiConsole.Confirm("Is there a lit indicator with the label FRK? ", false)
            do! updateBomb (fun bomb -> { bomb with LitFRK = Some litFRK})
            return litFRK
        | Some f -> return f
    }
let lastSNDigitOdd() =
    defuser { 
        let! digit = lastSNDigit()
        return digit &&& 1 = 1
    }

let threeWires colors =
    defuser {
        let numberOfColors = 
            colors
            |> Array.countBy id
            |> Map.ofArray
        
        if numberOf Color.Red numberOfColors=0 then
            return 2
        elif colors[2]=White then
            return 3
        elif numberOf Blue numberOfColors>1 then
            return lastIndexOf Blue colors + 1
        else
            return 3
    }

let fourWires colors =
    defuser {
        let numberOfColors = 
            colors
            |> Array.countBy id
            |> Map.ofArray
        
        let! isFirstCase = ~~(numberOf Red numberOfColors > 1) <&&> lastSNDigitOdd()
        if isFirstCase then
            return lastIndexOf Red colors + 1
        elif (colors[3]=Yellow && numberOf Red numberOfColors = 0) || (numberOf Blue numberOfColors = 1) then
            return 1
        elif numberOf Yellow numberOfColors > 1 then
            return 4
        else
            return 2
    }


let fiveWires colors =
    defuser {
        let numberOfColors = 
            colors
            |> Array.countBy id
            |> Map.ofArray

        let! isFirstCase = ~~ (colors[4] = Black) <&&> lastSNDigitOdd()
        if isFirstCase then
            return 4
        elif numberOf Red numberOfColors = 1 && numberOf Yellow numberOfColors > 1 then
            return 1
        elif numberOf Black numberOfColors = 0 then
            return 2
        else
            return 1
    }


let sixWires colors =
    defuser {
        let numberOfColors = 
            colors
            |> Array.countBy id
            |> Map.ofArray

        let! isFirstCase = ~~ (numberOf Yellow numberOfColors = 0) <&&> lastSNDigitOdd ()
        if  isFirstCase then
            return 3
        elif numberOf Yellow numberOfColors = 1 && numberOf White numberOfColors > 1 then
            return 4
        elif numberOf Red numberOfColors = 0 then
            return 6
        else
            return 4
    }

let printWire (colors: Color array) i =
    let color = Color.ConsoleColor colors[i]
    AnsiConsole.MarkupLine $"Cut wire [{color}]{i}[/]"

let rec solveWires() = 
    defuser {
        let colors = AnsiConsole.Prompt<string>(TextPrompt( "What are the wire colors? ") )
        match (run pColors colors) with
        | Failure (e,_,_) -> 
            AnsiConsole.MarkupLine($"[red]Incorrect input[/] : {e}")
            return! solveWires()
        | Success( r,_,_) when r.Length < 3 || r.Length > 6 -> 
            AnsiConsole.MarkupLine($"[red]Incorrect number of wires[/]")
            return! solveWires()
        | Success (r,_,_) ->
            let colorCodes = r |> List.toArray
            let! wireToCut = 
                match colorCodes.Length with
                | 3 -> threeWires colorCodes
                | 4 -> fourWires colorCodes
                | 5 -> fiveWires colorCodes
                | 6 -> sixWires colorCodes
                | _ -> failwith "this should not happen"
            printWire colorCodes wireToCut
    }

let moreBatteries n =
    defuser {
        let! bats = getBatteries()
        return bats > n
    }

let solveButton() =
    defuser {
        let input = AnsiConsole.Ask "What's the color of the button? "
        let color = oneColorIn input
        let text = AnsiConsole.Ask "What's written on the button? "
        let! hold =
            defuser {
                if color = Blue && (text =-= "Abort" || text =-= "Annuler") then
                    return true
                else
                    let! secondCase = ~~(text =-= "Detonate" || text =-= "Exploser") <&&> moreBatteries 1
                    if secondCase then
                        return false
                    else
                        let! isWhiteLitCar = ~~(color = White) <&&> getLitCAR()
                        if isWhiteLitCar then
                            return true
                        else
                            let! moreThanTwoBatsAndFrk = moreBatteries 2 <&&> getLitFRK()
                            if moreThanTwoBatsAndFrk then
                                return false
                            elif color = Yellow then
                                return true
                            elif color = Red && (text =-= "Hold" || text =-= "Maintenir") then
                                return false
                            else
                                return true
            }
        if hold then
            AnsiConsole.MarkupLine "Press and [green]hold[/] the button."
            let input = AnsiConsole.Ask<string> "After few seconds, a strip will light up. What is its color? "
            let stripcolor = oneColorIn input
            match stripcolor with
            | Blue -> AnsiConsole.MarkupLine "Release when there's a [green]4[/] somewhere on the timer."
            | Yellow ->  AnsiConsole.MarkupLine "Release when there's a [green]5[/] somewhere on the timer."
            | _ -> AnsiConsole.MarkupLine "Release when there's a [green]1[/] somewhere on the timer."
        else
            AnsiConsole.MarkupLine "Press the button and [red]immediatly release it[/]"

    }

let containsAll array elements =
    Array.fold(fun s e -> s && Array.contains e array) true elements

let getSymbols text = 
    let parsed = run pSymbols text
    match parsed with
    | Failure (e,_,_) -> failwith $"Bad input : {e}"
    | Success (r,_,_) -> Array.ofList r

let solveKeypad() =
    defuser {
        let columns = [|[|'Ϙ'; 'Ѧ'; 'ƛ'; 'Ϟ'; 'Ѭ'; 'ϗ'; 'Ͼ'|]
                        [|'Ӭ'; 'Ϙ'; 'Ͼ'; 'Ҩ'; '★'; 'ϗ'; '¿'|]
                        [|'©'; 'Ѽ'; 'Ҩ'; 'Җ'; 'Ԇ'; 'ƛ'; '★'|]
                        [|'б'; '¶'; 'Ѣ'; 'Ѭ'; 'Җ'; '¿'; 'ټ'|]
                        [|'Ψ'; 'ټ'; 'Ѣ'; 'Ͼ'; '¶'; 'Ѯ'; '★'|]
                        [|'б'; 'Ӭ'; '҂'; 'æ'; 'Ψ'; 'Ҋ'; 'Ω'|]|]
        AnsiConsole.MarkupLine """Symbol names :
Ϙ:Q        ©:Co     б:6     Ψ:Psi      Ѧ:At     Ѽ:W
¶:P        ټ:Sm     Ӭ:E     ƛ:Y        Ͽ/Ͼ:C    Ҩ:Ce
Ѣ:Bt       ҂:++     Ϟ:Z     Җ:Kk       Ѭ:Sp     æ:Ae
☆/★:St    Ԇ:R      ϗ:H     ¿:?        Ѯ:3      Ҋ:N   
Ω:Om"""
        let symbols = AnsiConsole.Ask "What are all the symbols (use the names over)? " |> getSymbols
         
        let mutable col = columns[0]
        for c in columns do
            if containsAll c symbols then
                col <- c
        let inOrder = Array.sortBy(fun e -> Array.IndexOf (col,e) ) symbols
        AnsiConsole.MarkupLine $"Press buttons in this order : [green]{inOrder[0]} {inOrder[1]} {inOrder[2]} {inOrder[3]}[/]"
    }






let simonWithVoyel = [|[|SBlue;SRed;SYellow;SGreen|];[|SYellow;SGreen;SBlue;SRed|];[|SGreen;SRed;SYellow;SBlue|]|]

let simonWithoutVoyel = [|[|SBlue;SYellow;SGreen;SRed|];[|SRed;SBlue;SYellow;SGreen|];[|SYellow;SGreen;SBlue;SRed|]|]

let rec solveSimon() =
    defuser {
        let! voyel = hasVoyelInSN()
        let table = if voyel then
                        simonWithVoyel
                    else 
                        simonWithoutVoyel
        let moves = table[0]
        let text = AnsiConsole.Ask<string>("What are the flashing colors? ")
        
        if text =-= "D" then
            ()
        else
            let colorsIn = 
                match (run pSimonColors (text)) with
                | Failure (e,_,_) -> failwith $"Bad input : {e}"
                | Success (r,_,_) -> r
            AnsiConsole.MarkupLine "Press these colors : "
            for c in colorsIn do
                let color = moves[SimonColor.toInt c] 
                AnsiConsole.Markup $"[{color}]{color}[/] "

            AnsiConsole.MarkupLine ""
            return! solveSimon()
    }

let rec askNumberMemory (m:string) =
    let input= AnsiConsole.Ask<int> m
    if input > 4 || input < 1 then
        AnsiConsole.MarkupLine "[red]nope[/]"
        askNumberMemory m
    else
        input

let AskMemoryWithLabel l =
    let p = askNumberMemory $"Press the button with [green]label {l}[/] and tell its position : "
    p,l

let AskMemoryWithPosition p =
    let l = askNumberMemory $"Press the button in [yellow]position {p}[/] and tell its label : "
    p,l

let solveMemory() =
    defuser {
        let stage1p,stage1l =
            match askNumberMemory "What is the number [blue]on the screen[/] ? " with
            | 1|2 -> AskMemoryWithPosition 2
            | 3 -> AskMemoryWithPosition 3
            | _ -> AskMemoryWithPosition 4
        let stage2p,stage2l =
            match askNumberMemory "Now, what is the number [blue]on the screen[/] ? " with
            | 1 -> AskMemoryWithLabel 4
            | 3 -> AskMemoryWithPosition 1
            | _ -> AskMemoryWithPosition stage1p
        let _,stage3l =
            match askNumberMemory "Now, what is the number [blue]on the screen[/] ? " with
            | 1 -> AskMemoryWithLabel stage2l
            | 2 -> AskMemoryWithLabel stage1l
            | 3 -> AskMemoryWithPosition 3
            | _ -> AskMemoryWithLabel 4
        let stage4p,stage4l =
            match askNumberMemory "Now, what is the number [blue]on the screen[/] ? " with
            | 1 -> AskMemoryWithPosition stage1p
            | 2 -> AskMemoryWithPosition 1
            | _ -> AskMemoryWithPosition stage2p
        let finall =
            match askNumberMemory "Now, what is the number [blue]on the screen[/] ? " with
            | 1 -> stage1l
            | 2 -> stage2l
            | 3 -> stage4l
            | _ -> stage3l
        AnsiConsole.MarkupLine $"Finally, press the button with [green]label {finall}[/]"  
    }


let pWires = stringCIReturn "wires"  (Some solveWires)
let pButton = stringCIReturn "button" (Some solveButton)
let pKeypad = stringCIReturn "keypad" (Some solveKeypad) <|> stringCIReturn "symbols" (Some solveKeypad)
let pMemory = stringCIReturn "memory" (Some solveMemory)
let pSimon = stringCIReturn "simon" (Some solveSimon)
let pQuit : Parser<(unit -> Defuser<unit>) option, unit> = stringCIReturn "defused" None <|> stringCIReturn "d"  None

let pModule = pWires <|> pButton <|> pKeypad <|> pMemory <|> pSimon <|> pQuit


let rec ktane() =
    defuser {
        
        let input = AnsiConsole.Prompt(
            SelectionPrompt<string>(
                    Title = "Which module ?",
                    Mode = SelectionMode.Independent,
                    WrapAround = true)
                .EnableSearch()
                .AddChoices([ "Wires"; "Button"; "Keypad"; "Simon"; "Memory"; "Defuse" ])
        ) 
        match run pModule input with
        | Success(Some f,_,_) -> 
            do! f()
            do! ktane()
        | Success(None,_,_) ->
            AnsiConsole.MarkupLine "[green]GGs[/]"
        | Failure(_,_,_) -> 
            AnsiConsole.MarkupLine "[red]Unknown module[/]"
            do! ktane()
    }


ktane () Bomb.empty
