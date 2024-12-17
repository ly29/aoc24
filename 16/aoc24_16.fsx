open System
open System.Collections.Generic

let test = """###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############""".Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries) |> array2D

let test' = """#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################""".Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries) |> array2D
let test'' = """##########
#.......E#
#.##.#####
#..#.....#
##.#####.#
#S.......#
##########""" |> _.Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries) |> array2D
let test''' = """########################################################
#.........#.........#.........#.........#.........#...E#
#.........#.........#.........#.........#.........#....#
#....#....#....#....#....#....#....#....#....#....#....#
#....#....#....#....#....#....#....#....#....#....#....#
#....#....#....#....#....#....#....#....#....#....#....#
#....#....#....#....#....#....#....#....#....#....#....#
#....#.........#.........#.........#.........#.........#
#S...#.........#.........#.........#.........#.........#
########################################################""".Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries) |> array2D
let data = IO.File.ReadAllText($"{__SOURCE_DIRECTORY__}/input.txt").Split('\n',  StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries) |> array2D
// let deltas = [|
//     0, -1
//     1, 0
//     0, 1
//     -1, 0
// |]

let findStart (b: char array2d) =
    let rec loop x y : int * int =
        if y = b.GetLength(1) then loop (x + 1) 0
        elif x = b.GetLength(0) then -1, -1 
        elif b[x, y] = 'S' then x, y 
        else loop x (y + 1)
    let x, y = loop 0 0
    x, y

type Direction = Up | Down | Left | Right

// let add  (x, y) d =
//     match d with
//     | Up -> d, x, y - 1
//     | Down -> d, x, y + 1
//     | Left -> d, x - 1, 0
//     | Right -> d, x - 1, 0

let add  (x, y) d =
    match d with
    | Left -> x, y - 1
    | Right -> x , y + 1
    | Up -> x - 1 , y
    | Down -> x + 1, y
   

let deltas = [Up ; Down ; Left; Right]

type Point = int * int

let getValue (d: Collections.Generic.Dictionary<_, int>) k =
    match d.TryGetValue k with
    | true, v -> v
    | false, _ -> Int32.MaxValue - 100000
let treePrint (p: Dictionary<Direction * Point, int>)  (ch: char array2d)=
    let pd = p |> Seq.map (|KeyValue|) |> Seq.groupBy (fun ((_, p), v) -> p) |> dict
    for x in 0 .. ch.GetLength(0) - 1 do
        for y in 0 .. ch.GetLength(1) - 1 do
            match pd.ContainsKey (x,y)  with
            | true -> printf "%c" '~'
            | _ -> printf "%c" ch[x, y]
        printfn ""
    ch

let walk (b: char array2d) (start: int * int) =
    let cameFrom = Dictionary<Point, Point>()
    let gScore =  Dictionary<Direction * Point, int>()
    let fScore =  Dictionary<Direction * Point, int>()
    gScore[(Right, start)] <- 0
    fScore[(Right, start ) ] <- 0

    let rec loop (points: (Direction * Point) list) =
        // printfn "loop %A" (points.Length, cameFrom.Count)
        // treePrint gScore b |> ignore
        // Console.ReadKey() |> ignore
        let mutable c = 1_000_000_000
        match points |> List.distinct |> List.sortBy (getValue fScore ) with
        | [] -> 
            printfn "camefrom %A" cameFrom.Count
            // ()
            failwith ""
        | xs :: rest ->
            let dir, ((x, y) as curr) = xs
            let currentCh = b[x, y]
            if currentCh = 'E' then 
                printfn "curr "
                gScore[xs]
            else
                deltas 
                |> List.choose (fun d ->
                    let (x, y) as next = add curr d
       

                    if b[x, y] <> '#' then  
                        let tscore =
                            (getValue gScore (dir, curr)) + if d = dir then 1 else 1001

                        // printfn "%A" (tscore, getValue gScore (d,next),  d = dir, d, dir , next, curr )
                        if  tscore < (getValue gScore (d, next)) then
                            cameFrom[next] <- curr
                            gScore[(d, next)] <- tscore
                            fScore[(d, next)] <- tscore + c
                            c <- c - 1
                            Some (d, next)
                        else None
                    else 
                        None
                )
                |> function
                    | [] -> loop rest
                    | r -> loop (r @ rest)
    loop [ Right, start] |> printfn "%A"
    treePrint gScore b |> ignore

let part1 (board: char array2d ) =
    let start = findStart board
    walk board start
    

part1 test |> printfn "%A"
part1 test' |> printfn "%A"
part1 test'' |> printfn "%A"
// part1 test''' |> printfn "%A"

part1 data|> printfn "%A"

