module Day12

open System
open System.IO

module private TestData = 
    let input = File.ReadAllLines("Day12/input.txt")
    
type Node = 
    | Start
    | BigCave of string
    | SmallCave of string
    | End

module private Node = 
    let parse str = 
        match str with 
        | "start" -> Start
        | "end" -> End
        | x when Char.IsUpper(x[0]) -> BigCave str
        | _ -> SmallCave str

type Graph = Map<Node, Set<Node>>

module private Graph = 
    let parse (strs: string array) = 
        strs
        |> Array.map (fun s -> s.Split('-'))
        |> Array.map (fun a -> Node.parse a[0], Node.parse a[1])
        |> Array.map (
            fun edge -> 
                match edge with
                | s, t when s <> Start && t <> End -> [| (s, t); (t, s) |]
                | _ -> [| edge |]
        )
        |> Array.collect id
        |> Array.groupBy fst
        |> Array.map (fun (s, e) -> (s, e |> Seq.map snd |> Set.ofSeq))
        |> Map.ofArray

module Puzzle23 = 
    let rec countPaths graph curNode visited =
        match curNode with
        | End -> 1
        | SmallCave _ when visited |> Set.contains curNode -> 0
        | Start when visited |> Set.contains curNode -> 0
        | _ ->
            let targets = Map.find curNode graph 
            let newVisited = visited.Add curNode

            targets
            |> Set.toList
            |> List.map (fun t -> countPaths graph t newVisited)
            |> List.sum

    let solve input = 
        let graph = Graph.parse input
        countPaths graph Start (Set<Node> [])

    let result = solve TestData.input