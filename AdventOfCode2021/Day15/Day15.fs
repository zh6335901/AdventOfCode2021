module Day15

open System.IO

module private TestData = 
    let charToInt (ch: char) = int ch - int '0'
    let riskLevels = 
        File.ReadAllLines("Day15/input.txt")
        |> Array.map (fun line -> line.ToCharArray() |> Array.map charToInt)

let inline min x y = if x > y then y else x

module Puzzle29 =
    let solve (riskLevels: int array array) = 
        let m, n = riskLevels.Length, riskLevels[0].Length
        let dp = Array.init m (fun _ -> Array.init n (fun _ -> 0))

        for i = 0 to (m - 1) do
            for j = 0 to (n - 1) do
                match i, j with
                | 0, 0 -> dp[0][0] <- 0
                | 0, c -> dp[0][c] <- dp[0][c - 1] + riskLevels[0][c]
                | r, 0 -> dp[r][0] <- dp[r - 1][0] + riskLevels[r][0]
                | r, c -> dp[r][c] <- (min (dp[r - 1][c]) (dp[r][c - 1])) + riskLevels[r][c]

        dp[m - 1][n - 1]

    let result = solve TestData.riskLevels

module Puzzle30 = 
    let solve (riskLevels: int array array) =
        let m, n = riskLevels.Length, riskLevels[0].Length
        let em, en = m * 5, n * 5
        
        let getRiskLevel i j = 
            let numOfRow, remainOfRow = i / m, i % m
            let numOfCol, remainOfCol = j / n, j % n
            let level = riskLevels[remainOfRow][remainOfCol] + numOfRow + numOfCol

            if level > 9 then level - 9 else level

        let dp = Array.init em (fun _ -> Array.init en (fun _ -> 0))

        for i = 0 to (em - 1) do
            for j = 0 to (en - 1) do
                match i, j with
                | 0, 0 -> dp[0][0] <- 0
                | 0, c -> dp[0][c] <- dp[0][c - 1] + getRiskLevel 0 c
                | r, 0 -> dp[r][0] <- dp[r - 1][0] + getRiskLevel r 0
                | r, c -> dp[r][c] <- (min (dp[r - 1][c]) (dp[r][c - 1])) + getRiskLevel r c

        dp[em - 1][en - 1]

    let result = solve TestData.riskLevels