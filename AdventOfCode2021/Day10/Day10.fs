module Day10

open System.IO

module private TestData = 
    let input = File.ReadAllLines("Day10/input.txt")

type Stack = char List

module private Stack = 
    let push (ch: char) (stack: Stack)  = ch :: stack
    let tryPeek (stack: Stack) = stack |> List.tryHead
    let pop (stack: Stack) = 
        match stack with
        | [] -> failwith "Stack underflow"
        | ch :: tail -> (tail, ch)

type LineState = { Line: string; Index: int; Stack: Stack }

module private Matching =
    let leftChars = [ '['; '('; '{'; '<']
    let rightChars = [ ']'; ')'; '}'; '>']
    let leftTable = (List.zip leftChars rightChars) |> Map.ofList
    let rightTable = (List.zip rightChars leftChars) |> Map.ofList

    let isLeftChar ch = leftChars |> List.contains ch
    let expectLeftChar ch = rightTable |> Map.find ch
    let expectRightChar ch = leftTable |> Map.find ch
    
    let matchLeftChar stack ch = 
        let expected = expectLeftChar ch
        let head = stack |> Stack.tryPeek
        match head with
        | Some e -> e = expected
        | None -> false                  

module Puzzle19 = 
    type Corruption = char Option

    let rec detectCorrupted { Line = line; Index = index; Stack = stack }: Corruption = 
        if index = line.Length then
            None
        else 
            let ch = line[index]
            if (Matching.isLeftChar ch) then 
                let ns = Stack.push ch stack 
                detectCorrupted { Line = line; Index = index + 1; Stack = ns }
            else
                let m = Matching.matchLeftChar stack ch
                if m then 
                    let ns, _ = Stack.pop stack
                    detectCorrupted { Line = line; Index = index + 1; Stack = ns }
                else
                    Some ch    

    let corruptionPointTable = (List.zip Matching.rightChars [ 57; 3; 1197; 25137 ]) |> Map.ofList

    let solve input = 
        let initLineState line = { Line = line; Index = 0; Stack = [] }
        let detect = initLineState >> detectCorrupted

        input 
        |> Array.map detect
        |> Array.filter Option.isSome
        |> Array.map (fun e -> corruptionPointTable |> Map.find (e |> Option.get))
        |> Array.sum

    let result = solve TestData.input

module Puzzle20 = 
    type Missing = char list option

    let rec detectMissing { Line = line; Index = index; Stack = stack }: Missing = 
        if index = line.Length then
            match stack with
            | [] -> None
            | x -> 
                let missingChars = x |> List.map (fun c -> Matching.expectRightChar c)
                Some missingChars
        else 
            let ch = line[index]
            if (Matching.isLeftChar ch) then 
                let ns = Stack.push ch stack 
                detectMissing { Line = line; Index = index + 1; Stack = ns }
            else
                let m = Matching.matchLeftChar stack ch
                if m then 
                    let ns, _ = Stack.pop stack
                    detectMissing { Line = line; Index = index + 1; Stack = ns }
                else
                    None

    let missingPointTable = (List.zip Matching.rightChars [ 2; 1; 3; 4 ]) |> Map.ofList

    let solve input = 
        let initLineState line = { Line = line; Index = 0; Stack = [] }
        let detect = initLineState >> detectMissing
        let calcPoints digits = 
            digits
            |> List.fold (fun s c -> s * 5L + c) 0L

        let missings =         
            input 
            |> Array.map detect
            |> Array.filter Option.isSome
            |> Array.map Option.get

        missings
        |> Array.map (fun cs -> cs |> List.map (fun c -> missingPointTable |> Map.find c |> int64))
        |> Array.map calcPoints
        |> Array.sort
        |> Array.item ((missings |> Array.length) / 2)

    let result = solve TestData.input