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
        |> Array.map (fun (s, t) -> [| (s, t); (t, s) |])
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
            let newVisited = visited.Add curNode
            let targets = Map.tryFind curNode graph |> Option.defaultValue (Set []) 

            targets
            |> Set.toList
            |> List.map (fun t -> countPaths graph t newVisited)
            |> List.sum

    let solve input = 
        let graph = Graph.parse input
        countPaths graph Start (Set<Node> [])

    let result = solve TestData.input

module Puzzle24 = 
    let rec countPaths graph curNode visited hasTwice =
        match curNode with
        | End -> 1
        | SmallCave _ when hasTwice && visited |> Set.contains curNode -> 0
        | Start when visited |> Set.contains curNode -> 0
        | _ ->
            let hasTwice' = 
                if hasTwice then true
                else
                    match curNode with
                    | SmallCave _ -> curNode |> visited.Contains
                    | _ -> false
                
            let newVisited = visited.Add curNode
            let targets = Map.tryFind curNode graph |> Option.defaultValue (Set []) 

            targets
            |> Set.toList
            |> List.map (fun t -> countPaths graph t newVisited hasTwice')
            |> List.sum

    let solve input = 
        let graph = Graph.parse input
        countPaths graph Start (Set<Node> []) false

    let result = solve TestData.input