module Day16

open System
open System.IO

module private TestData = 
    let input = File.ReadAllText("Day16/input.txt")
    let binaryText = 
        input.ToCharArray()
        |> Array.map (fun c -> Convert.ToString(Convert.ToInt64(c.ToString(), 16), 2).PadLeft(4, '0'))
        |> String.concat ""

type OperatorType = 
    | Sum
    | Product
    | Min
    | Max
    | GreaterThan
    | LessThan
    | EqualTo

let parseOperatorType = 
    function
    | 0L -> Sum
    | 1L -> Product
    | 2L -> Min
    | 3L -> Max
    | 5L -> GreaterThan
    | 6L -> LessThan
    | 7L -> EqualTo
    | _  -> failwith "unknown type"

type Packet = 
    | Literal of {| Version: int64; Value: int64 |}
    | Operator of {| Version: int64; Type: OperatorType; Subpackets: Packet list |}

let binToDec (number: string) = Convert.ToInt64(number, 2)

let splitAt (len: int) (str: string) = 
    str.Substring(0, len), str.Substring(len)

let parseLiteralValue text = 
    let rec readDigits text = 
        let digit, remain = text |> splitAt 5
        let prefix, d = digit |> splitAt 1

        if prefix = "0" then
            d, remain
        else
            let ds, remain' = readDigits remain
            d + ds, remain'

    let digits, remain = readDigits text
    digits |> binToDec, remain

let rec parsePacket text = 
    let versionText, remain = text |> splitAt 3
    let version = versionText |> binToDec
    let packetTypeText, remain' = remain |> splitAt 3
    let packetType = packetTypeText |> binToDec

    if packetType = 4 then
        let value, remain'' = parseLiteralValue remain'
        Literal {| Version = version; Value = value |}, remain''
    else 
        let subpackets, remain'' = parseSubpackets remain'
        Operator 
            {| Version = version; 
               Type = parseOperatorType packetType; 
               Subpackets = subpackets |}, 
        remain''

and parseSubpackets text = 
    let rec parseByTotalLength text = 
        if text = "" then
            []
        else 
            let packet, remain = parsePacket text
            packet :: (parseByTotalLength remain)

    let rec parseByNumOfPackets num text = 
        if num = 0L then 
            [], text
        else 
            let packet, remain = parsePacket text
            let restPackets, remain' = parseByNumOfPackets (num - 1L) remain

            packet :: restPackets, remain'
    
    let lengthType, remain = text |> splitAt 1
    if lengthType = "0" then
        let totalLengthText, remain' = remain |> splitAt 15
        let totalLength = totalLengthText |> binToDec |> int
        let subpacketsText, remain'' = remain' |> splitAt totalLength
        let subpackets = parseByTotalLength subpacketsText

        subpackets, remain''
    else
        let numText, remain' = remain |> splitAt 11
        let num = numText |> binToDec
        
        parseByNumOfPackets num remain'

module Puzzle31 = 
    let rec sumOfVersions packet = 
        match packet with
        | Literal l -> l.Version
        | Operator o -> o.Version + (o.Subpackets |> List.sumBy (fun sp -> sumOfVersions sp))

    let solve binaryText = 
        let packet, _ = parsePacket binaryText

        sumOfVersions packet

    let result = solve TestData.binaryText

module Puzzle32 = 
    let rec eval packet = 
        let executeOp op subpackets = 
            let subpacketVals = subpackets |> List.map eval

            let compare subpacketVals comparer = 
                let a, b = List.head subpacketVals, List.last subpacketVals 
                in if (comparer a b) then 1 else 0

            match op with
            | Sum -> subpacketVals |> List.fold (+) 0L
            | Product -> subpacketVals |> List.fold (*) 1L
            | Min -> subpacketVals |> List.min
            | Max -> subpacketVals |> List.max
            | GreaterThan -> compare subpacketVals (>)
            | LessThan -> compare subpacketVals (<)
            | EqualTo -> compare subpacketVals (=)

        match packet with 
        | Literal l -> l.Value
        | Operator o -> executeOp o.Type o.Subpackets

    let solve binaryText = 
        let packet, _ = parsePacket binaryText

        eval packet

    let result = solve TestData.binaryText
            