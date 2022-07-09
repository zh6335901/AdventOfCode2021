module Day14

open System.IO

type Pair = char * char
type Rules = Map<Pair, char>
type Counting = (Pair * int64) array

module private TestData = 
    let input = File.ReadAllLines("Day14/input.txt") 
    let pairwise (str: string) = str.ToCharArray() |> Array.pairwise
    let pairs = input |> Array.head |> pairwise
    let rules = 
        input 
        |> Array.skip 2
        |> Array.map (fun r -> r.Split(" -> "))
        |> Array.map (fun a -> (a[0] |> pairwise |> Array.head, a[1][0]))
        |> Map.ofArray

let solve (rules: Rules) (pairs: Pair array) (count: int) = 
    let split pair = 
        let ele = rules |> Map.find pair
        (fst pair, ele), (ele, snd pair)

    let rec evolve (counting: Counting) stepNum = 
        if stepNum = count 
        then counting
        else 
            let counting' = 
                counting
                |> Array.map (fun (pair, count) -> 
                    let fst, snd = pair |> split in [| (fst, count); (snd, count) |])
                |> Array.collect id
                |> Array.groupBy fst
                |> Array.map (fun (p, vs) -> (p, vs |> Array.map snd |> Array.sum))

            evolve counting' (stepNum + 1)

    let initial = pairs |> Array.countBy id |> Array.map (fun (p, c) -> (p, int64 c))
    let final = evolve initial 0

    let fstChar = pairs |> Array.head |> fst
    let charCounts = 
        final 
        |> Array.map (fun (p, c) -> (snd p, c))
        |> Array.groupBy fst
        |> Array.map (fun (ch, cs) -> (cs |> Array.sumBy snd) + (if ch = fstChar then 1L else 0L))
        |> Array.sort 
    let min = charCounts |> Array.head
    let max = charCounts |> Array.last

    max - min

module Puzzle27 =  
    let result = solve TestData.rules TestData.pairs 10

module Puzzle28 =  
    let result = solve TestData.rules TestData.pairs 40