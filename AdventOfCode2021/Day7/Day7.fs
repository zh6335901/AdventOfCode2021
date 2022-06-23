module Day7

open System
open System.IO

module private TestData = 
    let input = File.ReadAllText("Day7/input.txt") //"16,1,2,0,4,2,7,1,2,14"
    let grabs = input.Split(",") |> Array.map int

module Puzzle13 = 
    let solve grabs = 
        let abs (num: int) = Math.Abs(num)

        let bestPos = 
            grabs 
            |> Array.sort 
            |> Array.item (grabs.Length / 2)

        grabs
        |> Array.map (fun p -> (p - bestPos) |> abs)
        |> Array.sum
         
    let result = solve TestData.grabs

module Puzzle14 = 
    let solve grabs = 
        let abs (num: int) = Math.Abs(num)

        let cost fromPos toPos = 
            let n = ((fromPos - toPos) |> abs) + 1
            n * (n - 1) / 2

        let sumCost pos = 
            grabs
            |> Array.map (fun p -> cost p (pos - 1))
            |> Array.sum

        grabs 
        |> Array.map double 
        |> Array.average
        |> (fun avg -> int avg, (int avg) + 1)
        |> (fun (l, r) -> [l |> sumCost; r |> sumCost])
        |> Seq.min

    let result = solve TestData.grabs