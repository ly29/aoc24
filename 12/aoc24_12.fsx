let test = """RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE""".Split('\n') |> Array.map Array.ofSeq

let test' = """AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA""" |> _.Split('\n') |> Array.map Array.ofSeq

let testE = """EEEEE
EXXXX
EEEEE
EXXXX
EEEEE""" |> _.Split('\n') |> Array.map Array.ofSeq
let data = System.IO.File.ReadAllLines($"{__SOURCE_DIRECTORY__}/input.txt") |> Array.map Array.ofSeq

let deltas = [ 
    0, 1
    0, -1
    1, 0
    -1, 0
]

let getPos a (x, y) =
    a
    |> Array.tryItem x
    |> Option.bind (Array.tryItem y)
let addPos (x,y) (dx, dy) = x + dx, y + dy
let findRegion (a: char array array) (x, y) =
    let r = a[x][y]
    let rec loop (region: Set<int*int>) xy =
        match getPos a xy with
        | None -> region
        | Some ch when ch <> r -> region
        | Some _ ->
            let r' = region |> Set.add xy
            deltas
            |> List.map (addPos xy)
            |> List.filter (not << r'.Contains)
            |> List.fold loop r'

    loop Set.empty (x, y)

findRegion test (9, 0)
|> Set.count

let regions (map: char array array) = 
    let mapCopy = map |> Array.map (Array.length >> (fun c -> Array.create c -1))
    let mutable ri = 0
    [|
        for x in 0 .. map.Length - 1 do
            for y in 0 .. map[0].Length - 1 do
                if mapCopy[x][y] = -1 then
                    let r = findRegion map (x, y)
                    r |> Set.iter (fun (x, y) -> mapCopy[x][y] <- ri)
                    ri <- ri + 1
                    r
    |], mapCopy

let partOne (map: char array array) =
    let regions, newMap = regions map

    regions
    |> Array.indexed
    |> Array.sumBy (fun (i, s) ->
        let minx = s |> Seq.map fst |> Seq.min 
        let maxx = s |> Seq.map fst |> Seq.max 
        let miny = s |> Seq.map snd |> Seq.min 
        let maxy = s |> Seq.map snd |> Seq.max 

        [
            for x in minx - 1 .. maxx + 1 do 
                for y in miny - 1 .. maxy + 1  do
                    match getPos newMap (x, y) with
                    | Some n when i = n -> 
                        ()
                    | _ -> 
                        deltas
                        |> List.map (fun (dx, dy) -> x + dx, y + dy)
                        |> List.sumBy (fun pos -> 
                            match getPos newMap pos with
                            | Some n when n = i -> 1
                            | _ -> 0                        
                        )
        ]
        |> List.sum
        |> fun n -> n * (Set.count s)
    )
let transpose (a: 'a array2d) =
    [|
        for x in 0 .. a.GetLength(1) - 1 do
        [|
            for y in 0 .. a.GetLength(0) - 1 do
                a[y, x]
        |]
    |]
    |> array2D

let partTwo (map: char array array) =
    let regions, newMap = regions map

    regions
    |> Array.indexed
    |> Array.map (fun (i, s) ->
        let minx = s |> Seq.map fst |> Seq.min 
        let maxx = s |> Seq.map fst |> Seq.max 
        let miny = s |> Seq.map snd |> Seq.min 
        let maxy = s |> Seq.map snd |> Seq.max 

        [|

            for x in minx - 1 .. maxx + 1 do 
                [|
                for y in miny - 1 .. maxy + 1  do
                    if s.Contains (x, y) then 1 else 0
                |]
        |]

    )
    |> Array.map (fun a -> 
        let collectCorners (m : int array2d)=
            let d = [ -1, 0 ; -1 , -1 ; 0, 1]
            let dc = [ -1, 0; 0 , -1  ]
            [
                for x in 1 .. m.GetLength(0) - 2 do [
                    for y in 1 .. m.GetLength(1) - 2 do
                        let corner = 
                            d 
                            |> List.map (addPos (x, y))
                            |> List.forall (fun (t, u) -> m[t, u] = 0 )
                        let corner' = 
                            dc 
                            |> List.map (addPos (x, y))
                            |> List.forall (fun (t, u) -> m[t, u] = 1 )
                            |> fun c -> c && m[ x - 1, y - 1] = 0
                        if m[x, y] = 1 && (corner || corner' ) then 1 else 0
                ]
            ]
            |> array2D 
        let a2 = array2D a
        let a2' = a2 |> transpose 
        let a2'' = a2' |> transpose 
        let a2''' = a2'' |> transpose 
        [ a2 ; a2' ; a2'' ; a2''']
        |> List.map collectCorners
    )           

let timeIt f  x =
    let sw = System.Diagnostics.Stopwatch()
    sw.Start()
    let r = f x
    printfn "%O" sw.ElapsedMilliseconds
    r

partOne test
|> printfn "%A"

data
|> timeIt partOne 
|> printfn "%A"

testE
|> timeIt partTwo 
|> printfn "%A"