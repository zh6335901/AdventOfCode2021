module Day1

open System.IO

let input = File.ReadAllLines("Day1/input.txt")

module Puzzle1 =
    let isIncrease (n1, n2) = n2 > n1
    let count = 
        input
        |> Seq.map int 
        |> Seq.pairwise 
        |> Seq.filter isIncrease
        |> Seq.length

module Puzzle2 =
    let isIncrease (n1, n2) = (Seq.head n1) < (Seq.last n2)
    let count =
        input
        |> Seq.map int
        |> Seq.windowed 3
        |> Seq.pairwise 
        |> Seq.filter isIncrease
        |> Seq.length