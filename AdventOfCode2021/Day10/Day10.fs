module Day10

open System.IO

module private TestData = 
    let input = File.ReadAllLines("Day10/input.txt")

module private Matching =
    let leftChars = [ '('; '['; '{'; '<']
    let rightChars = [ ')'; ']'; '}'; '>']
    let rightTable = (List.zip rightChars leftChars) |> Map.ofList
    let isLeftChar ch = leftChars |> List.contains ch               

module Puzzle19 = 
    let corruptionPointTable = (List.zip Matching.rightChars [ 3; 57; 1197; 25137 ]) |> Map.ofList
    let getPoint ch = corruptionPointTable |> Map.find ch

    let rec score stack remain = 
        match stack, remain with
        | _, [] -> None
        | s, first :: last when first |> Matching.isLeftChar -> score (first :: s) last
        | [], first :: _ -> Some (getPoint first)
        | head :: tail, first :: last ->
            if head = (Matching.rightTable |> Map.find first) 
            then score tail last
            else Some (getPoint first)

    let solve (input: string[]) = 
        input 
        |> Array.map (fun l -> [], l.ToCharArray() |> List.ofArray)
        |> Array.choose (fun p -> score (fst p) (snd p))
        |> Array.sum

    let result = solve TestData.input

module Puzzle20 = 
    let missingPointTable = (List.zip Matching.leftChars [ 1; 2; 3; 4 ]) |> Map.ofList
    let getPoint cs =
        cs 
        |> List.map (fun c -> missingPointTable |> Map.find c |> int64)
        |> List.fold (fun s c -> s * 5L + c) 0L

    let rec score stack remain = 
        match stack, remain with
        | s, first :: last when first |> Matching.isLeftChar -> 
            score (first :: s) last
        | s, [] -> Some (getPoint s) 
        | [], _ -> None
        | head :: tail, first :: last ->
            if head = (Matching.rightTable |> Map.find first)
            then score tail last
            else None

    let solve (input: string[]) = 
        let missings = 
            input
            |> Array.map (fun l -> [], l.ToCharArray() |> List.ofArray)
            |> Array.choose (fun p -> score (fst p) (snd p))
            |> Array.sort

        missings |> Array.item ((missings |> Array.length) / 2)

    let result = solve TestData.input