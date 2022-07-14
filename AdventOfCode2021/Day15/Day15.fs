module Day15

open System
open System.IO
open System.Collections.Generic

module private TestData = 
    let charToInt (ch: char) = int ch - int '0'
    let riskLevels = 
        File.ReadAllLines("Day15/input.txt")
        |> Array.map (fun line -> line.ToCharArray() |> Array.map charToInt)

type Point = int * int

let createPriorityQueue () = PriorityQueue<Point, int>()
let dequeue (pq: PriorityQueue<Point, int>) = pq.Dequeue()
let enqueue (pq: PriorityQueue<Point, int>) point priority = pq.Enqueue(point, priority)

let solve (riskLevels: int array array) row col getRiskLevel = 
    let adjacent (x, y) = 
        [| (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) |]
        |> Array.filter (fun (r, c) -> r >= 0 && r < row && c >= 0 && c < col)

    let initDist x y = if x = 0 && y = 0 then 0 else Int32.MaxValue
    let distTo = Array.init row (fun r -> Array.init col (fun c -> initDist r c))
      
    let queue = createPriorityQueue ()
    enqueue queue (0, 0) 0

    let mutable found = false
    while not found do
        let x, y = dequeue queue
        if x = row - 1 && y = col - 1 then
            found <- true
        else
            let tos = adjacent (x, y)
            for tx, ty in tos do
                let d = distTo[x][y] + (getRiskLevel riskLevels tx ty)
                if d < distTo[tx][ty] then
                    distTo[tx][ty] <- d
                    enqueue queue (tx, ty) d

    distTo[row - 1][col - 1]

module Puzzle29 = 
    let row, col = let rl = TestData.riskLevels in (rl.Length, rl[0].Length)
    let result = solve TestData.riskLevels row col (fun riskLevels x y -> riskLevels[x][y])

module Puzzle30 = 
    let row, col = let rl = TestData.riskLevels in (rl.Length, rl[0].Length)
    let getRiskLevel (riskLevels: int array array) x y =
        let m, n = riskLevels.Length, riskLevels[0].Length
        let numOfRow, remainOfRow = x / m, x % m
        let numOfCol, remainOfCol = y / n, y % n
        let level = riskLevels[remainOfRow][remainOfCol] + numOfRow + numOfCol

        if level > 9 then level - 9 else level

    let result = solve TestData.riskLevels (row * 5) (col * 5) getRiskLevel