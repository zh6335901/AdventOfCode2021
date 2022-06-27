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
    let mutable visited = Set<int * int> []

    let rec countOfBasins x y (heightMap: int[][]) = 
        if Set.contains (x, y) visited then 
            0
        else
            visited <- Set.add (x, y) visited
            let sum = 
                adjacent x y heightMap
                |> Array.filter (fun (r, c) -> heightMap[r][c] <> 9)
                |> Array.sumBy (fun (r, c) -> countOfBasins r c heightMap)

            sum + 1
       
    let solve heightMap = 
        let lowPoints = getLowPoints heightMap
        let mutable total: int list = []

        for (x, y) in lowPoints do
            let count = countOfBasins x y heightMap
            total <- count :: total
            visited <- Set<int * int> []

        total
        |> List.sortDescending
        |> List.take 3
        |> List.reduce (fun c n -> c * n)

    let result = solve TestData.hegihtMap