module Day17

open System.IO

type Area = { Up: int; Down: int; Left: int; Right: int; }
type Coord = int * int
type Velocity = int * int

module private TestData = 
    let input = File.ReadAllText("Day17/input.txt")
    let area = 
        input.Replace("target area: ", "").Split(", ")
        |> Array.map (fun s -> s.Split("=") |> Array.last)
        |> Array.map (fun s -> s.Split(".."))
        |> Array.collect id
        |> (fun a -> { Up = int a[3]; Down = int a[2]; Left = int a[0]; Right = int a[1] })

let beyond area coord = 
    let x, y = coord
    x > area.Right || y < area.Down

let within area coord = 
    let x, y = coord
    x >= area.Left && x <= area.Right && y <= area.Up && y >= area.Down

let next coord velocity : (Coord * Velocity) = 
    let x, y = coord
    let vx, vy = velocity
    let x', y' = x + vx, y + vy
    let vx' = if vx = 0 then 0 else vx - 1
    let vy' = vy - 1

    (x', y'), (vx', vy')

let find area = 
    let rec track coord velocity highest = 
        if (beyond area coord) then
            None
        elif (within area coord) then
            Some highest
        else
            let coord', velocity' = next coord velocity
            let high = snd coord'
            let highest' = if highest < high then high else highest

            track coord' velocity' highest'

    [for vx in 1..area.Right do
        for vy in area.Down..300 -> track (0, 0) (vx, vy) 0]
    |> Seq.choose id

module Puzzle33 = 
    let solve area = find area |> Seq.max
    let result = solve TestData.area

module Puzzle34 = 
    let solve area = find area |> Seq.length
    let result = solve TestData.area