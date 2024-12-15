#r "nuget: MathNet.Numerics.FSharp"
open System
open MathNet.Numerics.LinearAlgebra
let test = """Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279""".Split('\n', StringSplitOptions.RemoveEmptyEntries)

let data = IO.File.ReadAllText($"{__SOURCE_DIRECTORY__}/input.txt").Split('\n', StringSplitOptions.RemoveEmptyEntries)

type Game = {
    A: int64 array
    B: int64 array 
    P: int64 array
}
let evalGame g an bn =
    let px = g.A[0] * an + g.B[0] * bn
    let py = g.A[1] * an + g.B[1] * bn
    px = g.P[0] && g.P[1] = py

let getNumbers (s: string) = 
    let m = Text.RegularExpressions.Regex.Matches(s, "\\d+")
    [| m[0].Value ; m[1].Value |] |> Array.map int64
let parse pfactor (a: string array) =
    [|
        for i in 0 .. 3 .. a.Length - 1 do {
                A = getNumbers a[i]
                B = getNumbers a[i + 1]
                P = getNumbers a[i + 2] |> Array.map ((+) pfactor)
            }
    |]
let run (g: Game ) = 
    [
        for an in 0L .. 100L do
            for bn in 0L .. 100L do
                if evalGame g an bn then
                    an * 3L + bn
    ]
    |> function
        | [] -> 0L
        | x -> List.min x    

parse 0L test
|> Array.map run
|> Array.sum

parse 0L data
|> Array.map run
|> Array.sum
|> printfn "%A"

let run2 (g: Game) =
    let m = matrix [|
        [| float g.A[0] ; float g.B[0]  |]
        [| float g.A[1] ; float g.B[1]  |]
    |]
    let b = vector [| float g.P[0] ; float g.P[1]|]
    let r = m.Solve(b)
    let an, bn = int64 (round r[0]), int64 (round r[1])
    if evalGame g an bn then
        (3L * an +  bn) |> Some
    else None

let factor2nd = 10000000000000L
parse factor2nd test
|> Array.map run2
|> printfn "%A"

parse factor2nd data
|> Array.choose run2
|> Array.sum
|> printfn "%A"