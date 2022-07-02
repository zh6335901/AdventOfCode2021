module Day11

open System.IO

module private TestData = 
    let input = File.ReadAllLines("Day11/input.txt")
    let energyLevels =
        input
        |> Array.map (fun l -> l.ToCharArray())
        |> Array.map (fun cs -> cs |> Array.map (fun c -> int c - int '0'))

let adjacent (levels: int array array) i j = 
    let m, n = levels.Length, levels[0].Length
    [| 
        (i - 1, j); 
        (i + 1, j); 
        (i, j - 1); 
        (i, j + 1);
        (i + 1, j + 1);
        (i - 1, j + 1);
        (i + 1, j - 1);
        (i - 1, j - 1);
    |]
    |> Array.filter (fun (r, c) -> r >= 0 && r < m && c >= 0 && c < n)

let rec flash (levels: int array array) = 
    let m, n = levels.Length, levels[0].Length
    let incMap = 
        [| for i in 0..(m - 1) do
            for j in 0..(n - 1) -> i, j, levels[i][j] |]
        |> Array.map (fun (i, j, v) -> if v > 9 then (adjacent levels i j) else [||])
        |> Array.collect id
        |> Array.countBy id
        |> Map.ofArray

    let getInc i j = 
        incMap |> Map.tryFind (i, j) |> Option.defaultValue 0

    let next i j level = 
        match level with
        | x when x > 9 -> 0
        | x when x = 0 -> 0
        | x -> x + (getInc i j)

    if (incMap |> Map.count) = 0 then 
        levels
    else
        let nextLevels = 
            levels 
            |> Array.indexed
            |> Array.map (fun (i, ls) -> ls |> Array.indexed |> Array.map (fun (j, l) -> next i j l))

        flash nextLevels

let getFlashCount levels = 
    levels
    |> Array.collect id
    |> Array.sumBy (fun l -> if l = 0 then 1 else 0)

let forward levels =  
    levels
    |> Array.map (fun ls -> ls |> Array.map (fun l -> l + 1))

let nextStep = forward >> flash

module Puzzle21 = 
    let solve energyLevels times = 
        let mutable flashingCount = 0
        let mutable levels = energyLevels

        for i = 1 to times do
            levels <- nextStep levels
            flashingCount <- flashingCount + (getFlashCount levels)

        flashingCount

    let result = solve TestData.energyLevels 100

module Puzzle22 = 
    let isAllFlashing levels = 
        not (levels |> Array.collect id |> Array.exists (fun l -> l > 0))

    let rec solve levels stepCount = 
        let newLevels = nextStep levels
        let curStepCount = stepCount + 1
        if isAllFlashing newLevels then
            curStepCount
        else
            solve newLevels curStepCount

    let result = solve TestData.energyLevels 0