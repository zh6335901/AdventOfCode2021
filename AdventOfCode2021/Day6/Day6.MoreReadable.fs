module Day6.MoreReadable

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
        |> Array.map (fun (k, g) -> Timer k, FishCount (g |> Array.length |> int64))
        |> Map.ofArray

let nextDay (state: State): State = 
    let parentFishCount = 
        state
        |> Map.tryFind (Timer 0)
        |> Option.defaultValue (FishCount 0)

    let tickDown (Timer t) = 
        if t = 0 then Timer 6
        else Timer (t - 1)

    let reducer (t, FishCount count1) (_, FishCount count2): (Timer * FishCount) = 
        (t, FishCount (count1 + count2))

    let next = 
        state 
        |> Map.toList
        |> List.map (fun (timer, count) -> (tickDown timer, count))
        |> List.groupBy (fun s -> fst s)
        |> List.map (fun (_, g) -> g |> List.reduce reducer)

    if parentFishCount = FishCount 0 then
        next |> Map.ofList
    else
        (Timer 8, parentFishCount) :: next |> Map.ofList

let rec nextDays state days = 
    if days = 0 then 
        state
    else 
        let nextState = nextDay state
        nextDays nextState (days - 1)

let solve state days = 
    nextDays state days 
    |> Map.toSeq 
    |> Seq.sumBy (fun (_, FishCount c) -> c)

module Puzzle11 = 
    let result = solve TestData.initState 80

module Puzzle12 = 
    let result = solve TestData.initState 256