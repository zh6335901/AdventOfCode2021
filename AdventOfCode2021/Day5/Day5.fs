module Day5

open System
open System.IO

type Point = int * int
type Line = { P1: Point; P2: Point }

module private TestData = 
    let input = File.ReadAllLines("Day5/input.txt")

    let parseLine (l: string): Line =
        let parsePoint (s: string) = 
            let parts = s.Split(",")
            (int parts[0], int parts[1])
            
        let parts = l.Split(" -> ")     
        { P1 = parts[0] |> parsePoint; P2 = parts[1] |> parsePoint }

    let lines = input |> Array.map parseLine


module Puzzle9 = 
    let isCardinal { P1 = (x1, y1); P2 = (x2, y2) } = 
        x1 = x2 || y1 = y2

    let generatePoints { P1 = (x1, y1); P2 = (x2, y2) } = 
        let minX, maxX = Math.Min(x1, x2), Math.Max(x1, x2)
        let minY, maxY = Math.Min(y1, y2), Math.Max(y1, y2)

        [| for x in minX..maxX do
            for y in minY..maxY -> (x, y) |]

    let solve lines = 
        lines 
        |> Array.filter isCardinal
        |> Array.map generatePoints
        |> Array.collect id
        |> Array.countBy id
        |> Array.filter (fun (_, count) -> count >= 2)
        |> Array.length

    let result = solve TestData.lines