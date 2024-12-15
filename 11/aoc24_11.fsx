open System.Collections.Generic
let t = "125 17".Split(' ') |> Array.map int64
let data = 
    System.IO.File.ReadAllText($"{__SOURCE_DIRECTORY__}/input.txt").Split(" ")
    |> Array.map int64

let timeIt f  x =
    let sw = System.Diagnostics.Stopwatch()
    sw.Start()
    let r = f x
    sw.Stop()
    printfn "ms %O ticks %i" sw.Elapsed.TotalMilliseconds sw.ElapsedTicks
    r
[<return: Struct>]
let (|Splitable|_|) (x: int64) =
    let xlog10 = int <| log10 (float x)
    if xlog10 % 2 = 1 then 
        let factor = int64 <| pown 10 (xlog10 / 2 + 1)
        ValueSome (x / factor, x % factor)
    else ValueNone

let rec calc (n, a: int64 ) = 
    let n' = n - 1
    match n, a with
    | 0, _ -> 1L
    | _, 0L -> calc (n', 1)
    | _, Splitable(x, y ) -> calc (n', x) + calc (n', y)
    | _, _ -> calc (n', 2024L * a )


let calcWithCache (cache: Dictionary<int * int64, int64>) n a = 
    let mutable cacheHits = 0L
    let mutable maxDepth = System.Int32.MaxValue

    let rec calc n (a: int64 ) =
        let key = (n, a) 
        let n' = n - 1
        maxDepth <- min maxDepth n
        match cache.TryGetValue key with
        | true, cn ->
            cacheHits <- cacheHits + 1L 
            cn
        | false, _ ->
            let res =
                match n, a with
                | 0, _ -> 1L 
                | _, 0L -> calc n' 1
                | _, Splitable(x, y ) -> (calc n' x) + (calc n' y)
                | _ -> calc n' (2024L * a)
            cache[key] <- res
            res
    let r = calc n a
    printfn "cache hits %i max depth %i" cacheHits maxDepth
    r

let cache = Dictionary<int * int64, int64>()
data
|> timeIt (Array.sumBy (fun a -> calcWithCache cache  25 a))
|> printfn "Part1: %i"

data
|> Array.sort
|> timeIt (Array.sumBy (calcWithCache cache 200))
|> printfn "Part2: %i"


printfn "cache size %i" cache.Count
printfn "unique numbers %i" (cache.Values |> Seq.distinct |> Seq.length)
