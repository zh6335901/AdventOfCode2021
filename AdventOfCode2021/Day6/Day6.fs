module Day6

open System.IO

module private TestData = 
    let input = File.ReadAllText("Day6/input.txt")
    let timers = input.Split(",") |> Array.map int

module private Timer = 
    let mutable cache = Map<int, int64> []

    let rec calc totalDays = 
        if Map.containsKey totalDays cache then
            cache |> Map.find totalDays
        else 
            let result = 
                [|for i in 0..7..totalDays
                    -> 1L + (calc (totalDays - i - 9)) |]
                |> Array.sum

            let newCache = cache |> Map.add totalDays result
            cache <- newCache

            result

let solve timers days = 
    let originCount = timers |> Array.length |> int64
        
    timers 
    |> Array.map (fun t -> Timer.calc (days - t - 1))
    |> Array.sum
    |> fun nc -> nc + originCount

module Puzzle11 = 
    let result = solve TestData.timers 80

module Puzzle12 = 
    let result = solve TestData.timers 256