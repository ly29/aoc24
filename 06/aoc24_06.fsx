open System.IO
open System.Collections.Generic

let test = """....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...""".Split('\n') |> Array.map Array.ofSeq
let data =
    File.ReadAllLines("input.txt")
    |>  Array.map Array.ofSeq
let getPos a (x, y) =
    a
    |> Array.tryItem y
    |> Option.bind (Array.tryItem x)

let deltas = [|
    0, -1
    1, 0
    0, 1
    -1, 0
|]
type Res = | Loop | Walked of HashSet<int * int * int>

let rec walk getPos test (x,y) (dir) (acc: (int *int * int) HashSet) =
    let (dx, dy) = deltas[dir]
    let x', y' = (x + dx, y + dy)
    match getPos test (x', y')  with
    | None ->
        acc.Add (x, y, dir) |> ignore
        Walked acc
    | Some '#' ->
        let d = (dir + 1) % deltas.Length
        if not <| acc.Add (x, y , dir) then Loop
        else
            walk getPos test (x, y) d acc
    | Some _ ->
        if not <| acc.Add ((x, y , dir) ) then Loop
        else
            walk getPos test (x', y') dir acc

let findStart (ground: char array array) =
    let x =ground |> Array.findIndex (Array.contains '^')
    ground[x] |> Array.findIndex ((=) '^'), x

let part1 getpos g=
    let (x, y) = findStart g

    walk getpos g (x, y) 0 (HashSet<int * int * int>())
    |> function
        | Loop -> Seq.empty
        | Walked s ->
            s
            |> Seq.map (fun (x, y, _) -> x, y)
    |> Seq.distinct

let walk2 (ground: char array array)  x  y =
    let f _ (x', y') =
        if x = x' && y = y' then Some '#'
        else getPos ground (x', y')
    let n = part1 f ground
    if Seq.isEmpty n then 1 else 0

let part2 (ground: char array array) =
    part1 getPos ground
    |> Seq.sumBy (fun (x, y) -> walk2 ground x y)

part1 getPos test
|> Seq.length
|> printfn "%A"

part1 getPos data
|> Seq.length
|> printfn "%A"

let timeIt f  x =
    let sw = System.Diagnostics.Stopwatch()
    sw.Start()
    let r = f x
    printfn "%O" sw.ElapsedMilliseconds
    r

part2 test
|> printfn "%A"

data
|> timeIt part2
|> printfn "%A"