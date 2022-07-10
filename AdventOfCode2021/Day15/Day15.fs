module Day15

open System.IO

module private TestData = 
    let charToInt (ch: char) = int ch - int '0'
    let riskLevels = 
        File.ReadAllLines("Day15/input.txt")
        |> Array.map (fun line -> line.ToCharArray() |> Array.map charToInt)

module Puzzle29 =
    let solve (riskLevels: int array array) = 
        let inline min x y = if x > y then y else x

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