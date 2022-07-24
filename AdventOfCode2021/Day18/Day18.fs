module Day18

open System
open System.IO

module private TestData = 
    let input = File.ReadAllLines("Day18/input.txt");

type SnailFishNumber = 
    | Pair of SnailFishNumber * SnailFishNumber
    | Regular of int

type Exploding = { LeftValue: int; RightValue: int; Position: int }

module private SnailFishNumber = 
    let private splitLine (line: string) = 
        let rec findSplitPos leftBrackets i = 
            match line[i] with
            | ',' when leftBrackets = 1 -> i
            | '[' -> findSplitPos (leftBrackets + 1) (i + 1)
            | ']' -> findSplitPos (leftBrackets - 1) (i + 1)
            | _ -> findSplitPos leftBrackets (i + 1)

        let pos = findSplitPos 0 0
        let left = line.Substring(1, pos - 1)
        let right = line.Substring(pos + 1, line.Length - pos - 2)
        
        (left, right)

    let rec parse (line: string) = 
        let (isInt, num) = Int32.TryParse(line)
        if isInt then 
            Regular num
        else
            let left, right = splitLine line
            Pair (parse left, parse right)

    let private explode fishNum = 
        let createExploding left right pos = 
            { LeftValue = left; RightValue = right; Position = pos }

        let rec explode' fishNum depth currentPos exploding = 
            match exploding with
            | Some _ -> fishNum, currentPos, exploding
            | None ->
                match fishNum with
                | Pair (Regular n1, Regular n2) when depth >= 4 -> 
                    Regular 0, currentPos, Some (createExploding n1 n2 currentPos)

                | Pair (n1, n2) -> 
                    let left, currentPos', exploding' = explode' n1 (depth + 1) currentPos None
                    let right, currentPos'', exploding'' = explode' n2 (depth + 1) currentPos' exploding'
                    Pair (left, right), currentPos'', exploding''

                | Regular _ -> fishNum, currentPos + 1, None

        let fishNum', _, exploding = explode' fishNum 0 0 None
        fishNum', exploding

    let private spreadExploding fishNum exploding = 
        let rec spread fishNum currentPos = 
            if currentPos > exploding.Position + 1 then
                fishNum, currentPos
            else
                match fishNum with
                | Regular n when currentPos = (exploding.Position - 1) -> Regular (n + exploding.LeftValue), currentPos + 1
                | Regular n when currentPos = (exploding.Position + 1) -> Regular (n + exploding.RightValue),  currentPos + 1
                | Regular _ -> fishNum, currentPos + 1 
                | Pair (n1, n2) -> 
                    let left, currentPos' = spread n1 currentPos
                    let right, currentPos'' = spread n2 currentPos'
                    Pair (left, right), currentPos''

        let fishNum', _ = spread fishNum 0
        fishNum'

    let rec private split fishNum splitted = 
        if splitted then
            fishNum, splitted
        else
            match fishNum with
            | Regular n when n >= 10 -> Pair (Regular (n / 2), Regular (n - n / 2)), true
            | Regular _ -> fishNum, false
            | Pair (n1, n2) ->
                let left, splitted' = split n1 false
                let right, splitted'' = split n2 splitted'
                Pair (left, right), splitted''

    let reduce fishNum = 
        let rec loop fishNum = 
            let explodedFishNum, exploding = explode fishNum
            let fishNum', reduced = 
                match exploding with
                | Some e -> spreadExploding explodedFishNum e, true
                | None -> split fishNum false

            if reduced then loop fishNum' else fishNum'

        loop fishNum      

    let add fishNum1 fishNum2 = 
        Pair (fishNum1, fishNum2) |> reduce

module Puzzle35 = 
    let rec evalSum fishNum = 
        match fishNum with
        | Regular n -> n
        | Pair (n1, n2) -> 3 * (evalSum n1) + 2 * (evalSum n2)

    let solve input = 
        input
        |> Array.map SnailFishNumber.parse
        |> Array.reduce SnailFishNumber.add
        |> evalSum

    let result = solve TestData.input