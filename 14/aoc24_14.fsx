#r "nuget: Plotly.NET"
#r "nuget: Plotly.NET.Interactive"
#r "nuget: Plotly.NET.ImageExport"
#r "nuget: MathNet.Numerics.FSharp"

open System.IO
open System.Text.RegularExpressions
open Plotly.NET
open Plotly.NET.ImageExport
open MathNet.Numerics.Statistics

let getNumbers (s: string) = 
    let m = Regex.Matches(s, "-?\\d+")
    [| m[0].Value ; m[1].Value ; m[2].Value ; m[3].Value |] |> Array.map int64
let test = """p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3""".Split('\n') |> Array.map getNumbers

let data = File.ReadAllLines($"{__SOURCE_DIRECTORY__}/input.txt") |> Array.map getNumbers
let testBoard =(7L, 11L)
let realBoard = (101L, 103L)
let timeIt f x =
    let sw = System.Diagnostics.Stopwatch()
    sw.Start()
    let r = f x
    printfn "ms %O" sw.ElapsedMilliseconds
    r
let (|Lt|_|) a b = if a > b then Some () else None
let (|Gt|_|) a b = if a < b then Some () else None
let move (bx, by) n (robot: int64 array) =
    let x' = (robot[0] + robot[2] * n ) % bx
    let y' = (robot[1] + robot[3] * n ) % by
    (if x' < 0 then bx + x' else x'), (if  y' < 0 then by + y' else y')
let classify (bx, by) (x', y')=
    let mx = bx / 2L
    let my = by / 2L
    match x', y' with
    | Gt mx, Gt my -> 1
    | Gt mx, Lt my -> 2
    | Lt mx, Gt my -> 3
    | Lt mx, Lt my -> 4
    | _ -> 0

let part1 board input =
    input
    |> Array.map (move board 100)
    |> Array.map (classify board)
    |> Array.filter ((<>) 0)
    |> Array.groupBy id
    |> Array.fold (fun acc (_, curr) -> curr.Length * acc) 1

part1 testBoard test |> printfn "%A"
part1 realBoard data |> printfn "%A"

let render board input n =
    input
    |> Array.map (move board n)   
    |> Array.distinct
    |> fun  a -> 
        let xs, ys = a|> Array.unzip
        let ch =
            Chart.Scatter(xs, ys, mode=StyleParam.Mode.Markers) 
            |> Chart.withTitle $"{n}"
        ch
        |> Chart.savePNG($"{__SOURCE_DIRECTORY__}/out/output_{n}", Width = 1000, Height = 1000)
        ch
        |> Chart.withSize(1000, 1000)
        |> Chart.show

// visual approach 
// let part2viz ((bx, by) as board) input n step np =
//     for x in n .. step .. np do
//         render board input x
    
// part2viz (101, 103) data 0 1 100 etc

let part2  (bx, by) data =
    [  for n in 0L .. bx * by do 
        let cx, cy = data |> Array.map (move (bx, by) n) |> Array.unzip
        Statistics.Variance(cx |> Seq.map float) + Statistics.Variance(cy |> Seq.map float), n
    ]
    |> List.minBy fst
    |> snd

data
|> timeIt (part2 realBoard)
|> (int64 >> render realBoard data)
