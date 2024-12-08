open System.IO
let test = """............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............""".Split('\n')
let data = File.ReadAllLines("input.txt")

let getAntennas (test: string array) = 
    let ext (_, (x, y)) = x, y
    [|
        for y in 0 .. test.Length - 1 do
            for x in  0 .. test[0].Length - 1 do
                if test[y][x] <> '.' then
                    test[y][x], (x, y) 
    |] 
    |> Array.groupBy fst
    |> Array.map (snd >> Array.map ext) 

let calc (x0, y0) (x1, y1) =
    let dx, dy = x1 - x0, y1 - y0
    [
        x0 - dx, y0 - dy
        x1 + dx, y1 + dy
    ]

let mapCombo calc (t: _ array array) =
    t
    |> Array.collect (fun a ->
        [|
            for x in 0 .. a.Length - 1 do
                for y in x + 1 .. a.Length - 1 do
                    yield! calc a[x] a[y]
        |])

let findPos1 (map: string array) =
    let maxX, maxY = map.Length, map[0].Length
    getAntennas map
    |> mapCombo calc
    |> Array.filter (fun (x, y ) -> x >= 0 && x < maxX && y >= 0 && y < maxY)
    |> Array.distinct
    |> Array.length

let findPos2 (map: string array) =
    let maxX, maxY = map.Length, map[0].Length

    let genAntis (x, y) (dx, dy) =
        let rec loop (x, y ) acc =
            let (x', y') as next= x - dx, y - dy 
            if x' >= 0 && y' >= 0 && x' < maxX && y' < maxY 
            then loop next (next :: acc)
            else acc
        loop (x, y) [x, y]

    map
    |> getAntennas 
    |> mapCombo (fun (x0, y0)  (x1, y1)->
        [
            yield! genAntis (x0, y0) (x0 - x1, y0 - y1)
            yield! genAntis (x1, y1) (x1 - x0, y1 - y0)
        ]
    )
    |> Array.distinct
    |> Array.length 

let timeIt f  x =
    let sw = System.Diagnostics.Stopwatch()
    sw.Start()
    let r = f x
    sw.Stop()
    printfn "us %O ticks %i" sw.Elapsed.TotalMicroseconds sw.ElapsedTicks
    r

test |> findPos1 |> printfn "Test 1: %A"
data |> timeIt findPos1 |> printfn "Data 1: %A"
test |> findPos2 |> printfn "Test 2: %A"
data |> timeIt findPos2 |> printfn "Data 2: %A"

