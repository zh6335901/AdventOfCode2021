module Day4

open System
open System.IO;

let input = File.ReadAllText("Day4/input.txt")
let regions = input.Split("\r\n\r\n")

let splitStr (separtor: string) (str: string) 
    = str.Split(separtor, StringSplitOptions.RemoveEmptyEntries) 

let drawnInput = regions[0]
let boardInputs = 
    regions 
    |> Array.skip 1
    |> Array.map (fun m -> m.Split("\r\n"))
    |> Array.map (fun m -> m |> Array.map (fun x -> x |> splitStr " "))

module Puzzle7 = 
    type Board = { Rows: Set<string> array; Cols: Set<string> array; Bingo: bool }

    let toBoard (input: string[][]) = 
        let (m, n) = (input.Length, input[0].Length)
        let martix = Array2D.init m n (fun i j -> input[i][j])

        let rows = 
            [|1..m|] 
            |> Array.map (fun r -> martix[r - 1, *])
            |> Array.map Set.ofArray

        let cols = 
            [|1..n|] 
            |> Array.map (fun c -> martix[*, c - 1])
            |> Array.map Set.ofArray

        { Rows = rows; Cols = cols; Bingo = false }

    let markBoard num board = 
        let rows = board.Rows |> Array.map (fun r -> r |> Set.remove num)
        let cols = board.Cols |> Array.map (fun c -> c |> Set.remove num)
        let bingo = 
            [rows; cols] 
            |> Array.concat 
            |> Array.exists (fun s -> s |> Set.isEmpty)

        { Rows = rows; Cols = cols; Bingo = bingo }

    let calcBoard board lastNum =
        let calcRow row = 
            row 
            |> Set.toArray
            |> Array.map int
            |> Array.fold (fun s n -> s + n) 0 

        let sum = 
            board.Rows
            |> Array.map calcRow
            |> Array.reduce (fun i j -> i + j)
        
        sum * (int lastNum)

    let rec calc boards drawnNumbers = 
        match drawnNumbers with
        | cur :: rest -> 
            let nextBoards = boards |> Array.map (markBoard cur)              
            let winner = nextBoards |> Array.tryFind (fun b -> b.Bingo)
            match winner with
            | Some x -> calcBoard x cur
            | None -> calc nextBoards rest 
        | _ -> 0

    let solve drawnInput boardInputs =
        let boards = boardInputs |> Array.map toBoard
        let drawnNumbers = 
            drawnInput
            |> splitStr ","
            |> List.ofArray

        calc boards drawnNumbers

    let result = solve drawnInput boardInputs