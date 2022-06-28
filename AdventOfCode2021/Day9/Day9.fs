module Day9

open System
open System.IO

module private TestData =  
    let input = File.ReadAllLines("day9/input.txt")

    let hegihtMap = 
        input
        |> Array.map (fun s -> s.ToCharArray() |> Array.map (fun c -> int c - int '0'))

let adjacent x y (heightMap: int[][]) = 
    let m, n = heightMap.Length, heightMap[0].Length

    [|(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)|]
    |> Array.filter (fun (r, c) -> r >= 0 && r < m && c >= 0 && c < n)

let isLowPoint x y (heightMap: int[][]) = 
    let v = heightMap[x][y]
    let distances = 
        (adjacent x y heightMap) 
        |> Array.map (fun (r, c) -> v - heightMap[r][c])

    Array.TrueForAll(distances, fun d -> d < 0)

let getLowPoints (heightMap: int[][]) = 
    let m, n = heightMap.Length, heightMap[0].Length   
    let lowPoints = 
        [|for i in 0..(m - 1) do 
            for j in 0..(n - 1) -> (i, j, isLowPoint i j heightMap) |]
        |> Array.filter (fun (_, _, d) -> d)
        |> Array.map (fun (i, j, _) -> i, j)

    lowPoints

module Puzzle17 =
    let solve heightMap = 
        let lowPoints = getLowPoints heightMap
        lowPoints
        |> Array.sumBy (fun (i, j) -> heightMap[i][j] + 1)

    let result = solve TestData.hegihtMap

module Puzzle18 =
    let rec expandBasin (heightMap: int[][]) basin x y  = 
        if basin |> Set.contains (x, y)  then 
            basin
        else
            let curBasin = basin |> Set.add (x, y)
            let finalBasin = 
                adjacent x y heightMap
                |> Array.filter (fun (r, c) -> heightMap[r][c] <> 9)
                |> Array.fold (fun b (r, c) -> expandBasin heightMap b r c) curBasin

            finalBasin
       
    let solve heightMap = 
        let lowPoints = getLowPoints heightMap
        let initBasin = Set<int * int> []
        let expand = expandBasin heightMap initBasin
        let counts = 
            lowPoints
            |> Array.map (fun (x, y) -> expand x y)
            |> Array.map (fun s -> s |> Set.count)

        counts
        |> Array.sortDescending
        |> Array.take 3
        |> Array.reduce (fun c n -> c * n)

    let result = solve TestData.hegihtMap