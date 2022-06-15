module Day3

open System.IO
open System

let input = File.ReadAllLines("Day3/input.txt")
    
module Puzzle5 =
    let initCounter: int list = [ for i in 1..input[0].Length -> 0 ]
    let total = input.Length

    let aggr counter binary =
        binary
        |> Seq.zip counter
        |> Seq.map (fun (i, ch) -> i + if ch = '1' then 1 else 0)

    let counter = input |> Seq.fold aggr initCounter

    let gammaBits = 
        counter
        |> Seq.map (fun c -> if c >= total - c  then '1' else '0')

    let epsilonBits = 
        gammaBits |> Seq.map (fun b -> if b = '1' then '0' else '1')

    let gs = String(gammaBits |> Seq.toArray)
    let es = String(epsilonBits |> Seq.toArray)

    let gammaNum = Convert.ToInt32(gs, 2)
    let epsilonNum = Convert.ToInt32(es, 2)

    let result = gammaNum * epsilonNum

module Puzzle6 =
    type Report = { Position: int; Numbers: seq<string> }
    type Criteria = Most | Least

    let filter criteria report = 
        let { Position = pos; Numbers = numbers } = report

        let total = Seq.length numbers
        let oneCount = numbers |> Seq.sumBy (fun n -> if n[pos] = '1' then 1 else 0)
        let zeroCount = total - oneCount

        let criteriaChar = 
            match criteria with
            | Most -> if oneCount >= zeroCount then '1' else '0'
            | Least -> if oneCount >= zeroCount then '0' else '1'

        let nextNumbers = 
            numbers 
            |> Seq.filter (fun n -> n[pos] = criteriaChar)

        { Position = pos + 1; Numbers = nextNumbers }

    let rec loopFilter criteria report = 
        let nextReport = filter criteria report
        match nextReport with
        | { Numbers = numbers } when (numbers |> Seq.length) = 1 -> nextReport
        | _ -> nextReport |> loopFilter criteria

    let initReport = { Position = 0; Numbers = input }
    let oxygenReport = initReport |> loopFilter Most
    let co2Report = initReport |> loopFilter Least

    let oxygenRating = Convert.ToInt32(oxygenReport.Numbers |> Seq.head, 2)
    let co2Rating = Convert.ToInt32(co2Report.Numbers |> Seq.head, 2)

    let result = oxygenRating * co2Rating