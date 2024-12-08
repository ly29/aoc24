open System.IO
open System

let test = """190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20""".Split('\n') |> Array.map (
        _.Split([|':';' '|], StringSplitOptions.RemoveEmptyEntries) >> (Array.map int64) )
let data =  
    File.ReadAllLines($"{__SOURCE_DIRECTORY__}/input.txt")
    |> Array.map (
        _.Split([|':';' '|], StringSplitOptions.RemoveEmptyEntries) >> (Array.map int64) )
let concat a b =
  let rec loop acc  = function
    | 0L -> acc + b
    | t -> loop (acc * 10L) (t / 10L)
  loop a b

let res = ResizeArray<int64>()
let timeIt f  x =
    let sw = System.Diagnostics.Stopwatch()
    sw.Start()
    let r = f x
    sw.Stop()
    printfn "Time taken %O %i" sw.ElapsedMilliseconds sw.ElapsedTicks
    res.Add(sw.ElapsedTicks)
    r

let eval (a: int64 array) =
    let rec loop (x: int) acc =
        if x = a.Length then acc = a[0]
        elif acc > a[0] then false
        elif (loop (x + 1) (acc * a[x])) then true
        elif (loop (x + 1) (acc + a[x])) then true
        else false
    loop 2 a[1]

let eval2 (a: int64 array) =
    let rec loop (x: int) acc =
        if x = a.Length then acc = a[0]
        elif acc > a[0] then false
        elif (loop (x + 1) (acc * a[x])) then true
        elif (loop (x + 1) (acc + a[x])) then true
        elif (loop (x + 1) (concat acc a[x])) then true
        else false
    loop 2 a[1]

// test
// |> Array.filter eval
// |> Array.sumBy Array.head

data
|> timeIt Array.filter eval
|> Array.sumBy Array.head
|> printfn "%i"

// test
// |> Array.filter eval2
// |> Array.sumBy Array.head

data
|> timeIt Array.filter eval2
|> Array.sumBy Array.head
|> printfn "%i"

float res[1] / float res[0]
|> printfn "%A"