module Day2

open System.IO

module private TestData =
    let input = File.ReadAllLines("Day2/input.txt")

module Puzzle3 = 
    let solve (input: string seq) = 
        input
        |> Seq.map (fun l -> l.Split(" "))
        |> Seq.map (fun a -> (a[0], int a[1]))
        |> Seq.map (
            fun t -> 
                match t with
                | ("forward", d) -> (d, 0)
                | ("down", d) -> (0, d)
                | ("up", d) -> (0, -d)
                | (_, _) -> (0, 0)
        )
        |> Seq.fold (fun (x, y) (dx, dy) -> (x + dx, y + dy)) (0, 0)
        |> fun (x, y) -> x * y

    let result = solve TestData.input

module Puzzle4 = 
    type Command = 
        | Forword of int
        | Down of int
        | Up of int

    let parseCommand (line: string): Command = 
        let arr = line.Split(" ")
        let commandText = arr[0]
        let distance = int arr[1]
        let command = 
            match commandText with
            | "forward" -> Forword
            | "down" -> Down
            | "up" -> Up
            | _ -> failwith "unknown command"

        distance |> command 

    type SubmarineState = { HorizontalPosition: int; Depth: int; Aim: int; }

    let applyCommand state command =
        match command with
        | Forword x -> 
            { state 
                with HorizontalPosition = state.HorizontalPosition + x
                     Depth = state.Depth + x * state.Aim }
        | Down x -> { state with Aim = state.Aim + x }
        | Up x -> { state with Aim = state.Aim - x }

    let solve input = 
        let initSubmarineState = { HorizontalPosition = 0; Depth = 0; Aim = 0; }

        input
        |> Seq.map parseCommand
        |> Seq.fold applyCommand initSubmarineState
        |> (fun state -> state.HorizontalPosition * state.Depth)

    let result = solve TestData.input
        