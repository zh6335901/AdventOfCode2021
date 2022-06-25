module Day8

open System.IO
open System

module private TestData = 
    let splitStr (separtor: string) (str: string) = 
        str.Split(separtor, StringSplitOptions.RemoveEmptyEntries)

    let getPart i parts = 
        parts 
        |> Array.map (Array.item i) 
        |> Array.map (splitStr " ")

    let lineParts = 
        File.ReadAllLines("Day8/input.txt") 
        |> Array.map (splitStr "|")

    let signalTable = lineParts |> getPart 0
    let digitTable = lineParts |> getPart 1

module Puzzle15 = 
    let solve (digits: string array array) = 
        digits
        |> Array.collect id
        |> Array.filter (fun d -> [|2; 4; 3; 7|] |> Array.contains d.Length)
        |> Array.length

    let result = solve TestData.digitTable

module Puzzle16 = 
    type Progress = string array * Map<string, int>

    let sortSignalChars (signal: string) =
        signal.ToCharArray()
        |> Array.sort
        |> (fun cs -> String(cs))

    let mapSignal signal num digitMap = 
        digitMap 
        |> Map.add (sortSignalChars signal) num

    let contains (value: string) (source: string) = 
        Seq.fold (fun b c -> if b then b && source.Contains(char c) else false) true value

    let findDigit num digitMap = 
        digitMap 
        |> Map.toSeq 
        |> Seq.find (fun (_, v) -> v = num) 
        |> fst

    let evolveProgress evolver progress = 
        let folder (signals: string array, digitMap: Map<string, int>) (signal: string) = 
            let num = evolver digitMap signal
            match num with
            | Some x -> signals |> Array.filter (fun s -> s <> signal), digitMap |> mapSignal signal x 
            | None -> signals, digitMap

        fst progress
        |> Seq.fold folder progress

    let evolver1478 _ (signal: string) = 
        match signal.Length with
        | 2 -> Some 1
        | 4 -> Some 4
        | 3 -> Some 7
        | 7 -> Some 8
        | _ -> None

    let evolver069 digitMap (signal: string) = 
        let digit1 = digitMap |> findDigit 1
        let digit4 = digitMap |> findDigit 4

        match signal with
        | x when x.Length <> 6 -> None
        | x when not (x |> contains digit1) -> Some 6
        | x when x |> contains digit4 -> Some 9
        | _ -> Some 0

    let evolver235 digitMap (signal: string) = 
        let digit1 = digitMap |> findDigit 1
        let digit6 = digitMap |> findDigit 6

        match signal with
        | x when x.Length <> 5 -> None
        | x when x |> contains digit1 -> Some 3
        | x when digit6 |> contains x -> Some 5
        | _ -> Some 2

    let evolve signals = 
        (signals, Map<string, int> []) 
        |> (evolveProgress evolver1478 >> evolveProgress evolver069 >> evolveProgress evolver235)
        |> snd

    let calc digitMap (digits: string array) = 
        digits
        |> Array.map sortSignalChars
        |> Array.map (fun k -> Map.find k digitMap)
        |> Array.fold (fun n d -> n * 10 + d) 0

    let solve signalTable digitTable = 
        signalTable
        |> Array.zip digitTable
        |> Array.map (fun (d, s) -> d |> calc (evolve s))
        |> Array.sum

    let result = solve TestData.signalTable TestData.digitTable