module Day6MoreReadable

open System.IO

type Timer = Timer of int
type FishCount = FishCount of int64
type State = Map<Timer, FishCount>

module private TestData = 
    let input = File.ReadAllText("Day6/input.txt")

    let initState = 
        input.Split(",") 
        |> Array.map int
        |> Array.groupBy id
        |> Array.map (fun (t, g) -> Timer t, FishCount (g |> Array.length |> int64))
        |> Map.ofArray

module private Timing = 
    let private (@+@) (FishCount c1) (FishCount c2) = FishCount (c1 + c2) 

    let nextDay (state: State): State = 
        let getFishCount (timerValue: int) = 
            state
            |> Map.tryFind (Timer timerValue)
            |> Option.defaultValue (FishCount 0)

        [0..8]
        |> List.map (
            fun t -> 
                match t with
                | 8 -> (Timer 8, getFishCount 0)
                | 6 -> (Timer 6, (getFishCount 0) @+@ (getFishCount 7))
                | _ -> (Timer t, getFishCount (t + 1))
        )
        |> Map.ofList
    
    let rec nextDays state days = 
        match days with
        | 0 -> state
        | _ ->         
            let nextState = nextDay state
            nextDays nextState (days - 1)

let solve state days = 
    Timing.nextDays state days 
    |> Map.toSeq 
    |> Seq.sumBy (fun (_, FishCount c) -> c)

module Puzzle11 = 
    let result = solve TestData.initState 80

module Puzzle12 = 
    let result = solve TestData.initState 256