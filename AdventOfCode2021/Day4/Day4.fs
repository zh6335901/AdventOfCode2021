module Day4

open System
open System.IO;

type Board = { Rows: Set<string> array; Cols: Set<string> array; Bingo: bool }

module private Board = 
    let ofStringTable (table: string[][]) = 
        let (m, n) = (table.Length, table[0].Length)
        let martix = Array2D.init m n (fun i j -> table[i][j])

        let rows = 
            [|1..m|] 
            |> Array.map (fun r -> martix[r - 1, *])
            |> Array.map Set.ofArray

        let cols = 
            [|1..n|] 
            |> Array.map (fun c -> martix[*, c - 1])
            |> Array.map Set.ofArray

        { Rows = rows; Cols = cols; Bingo = false }

    let mark num board = 
        let rows = board.Rows |> Array.map (fun r -> r |> Set.remove num)
        let cols = board.Cols |> Array.map (fun c -> c |> Set.remove num)
        let bingo = 
            [rows; cols] 
            |> Array.concat 
            |> Array.exists (fun s -> s |> Set.isEmpty)

        { Rows = rows; Cols = cols; Bingo = bingo }

    let calc lastNum board =
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

module private TestData =
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

    let boards = boardInputs |> Array.map Board.ofStringTable
    let drawnNumbers = 
        drawnInput
        |> splitStr ","
        |> List.ofArray

module Puzzle7 = 
    let rec solve drawnNumbers boards = 
        match drawnNumbers with
        | cur :: rest -> 
            let nextBoards = boards |> Array.map (Board.mark cur)              
            let winner = nextBoards |> Array.tryFind (fun b -> b.Bingo)
            match winner with
            | Some x -> Board.calc cur x
            | None -> solve rest nextBoards
        | _ -> 0

    let result = solve TestData.drawnNumbers TestData.boards

module Puzzle8 = 
    let rec solve drawnNumbers boards = 
        match drawnNumbers with
        | cur :: rest -> 
            let nextBoards = boards |> Array.map (Board.mark cur)              
            match nextBoards with
            | x when x.Length = 1 && x[0].Bingo = true -> Board.calc cur x[0]
            | _ -> nextBoards |> Array.filter (fun b -> b.Bingo = false) |> solve rest 
        | _ -> 0

    let result = solve TestData.drawnNumbers TestData.boards