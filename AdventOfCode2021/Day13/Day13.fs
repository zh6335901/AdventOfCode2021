module Day13

open System
open System.IO

module private TestData = 
    let splitStr (separtor: string) (str: string) 
        = str.Split(separtor, StringSplitOptions.RemoveEmptyEntries) 

    let input = File.ReadAllText("Day13/input.txt")
    let inputParts = input |> splitStr "\r\n\r\n"

    let points = 
        inputParts[0]
        |> splitStr "\r\n"
        |> Array.map (splitStr ",")
        |> Array.map (fun a -> int a[0], int a[1])

    let instructions = 
        inputParts[1]
        |> splitStr "\r\n"
        |> Array.map (fun i -> i.Replace("fold along ", "") |> splitStr "=")
        |> Array.map (fun i -> i[0], int i[1])

let overlap instruction point = 
    let px, py = point
    match instruction with 
    | "x", line when px > line -> (line * 2 - px, py)
    | "y", line when py > line -> (px, line * 2 - py)
    | _ -> point

module Puzzle25 = 
    let solve points instruction = 
        let overlappings = 
            points 
            |> Array.map (overlap instruction)
            |> Array.distinct
        
        overlappings |> Array.length

    let result = solve TestData.points TestData.instructions[0]

module Puzzle26 = 
    let solve points instructions = 
        let folder points instruction = 
            points 
            |> Array.map (overlap instruction)
            |> Array.distinct

        let overlappings = (points, instructions) ||> Array.fold folder
        let overlappingSet = overlappings |> Set.ofArray
        let maxX = overlappings |> Array.maxBy fst |> fst
        let maxY = overlappings |> Array.maxBy snd |> snd

        let image = 
            [| 0..maxY |]
            |> Array.map (
                fun y ->
                    [| 0..maxX |]
                    |> Array.map (fun x -> if overlappingSet.Contains (x, y) then "#" else ".")
                    |> String.concat ""
            )
            |> String.concat "\n"

        image

    let result = solve TestData.points TestData.instructions