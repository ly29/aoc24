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
   
let (|Replaceable|_|) (x: int64) =
    let xlog10 = int <| log10 (float x)
    if xlog10 % 2 = 1 then 
        let factor = int64 <| pown 10 (xlog10 / 2 + 1)
        Some (x / factor, x % factor)
    else None

let rec calc (n, a: int64 ) = 
    let n' = n - 1
    match n, a with
    | 0, _ -> 1L
    | _, 0L -> calc (n', 1)
    | _, Replaceable(x, y ) -> calc (n', x) + calc (n', y)
    | _, _ -> calc (n', 2024L * a )
 
data
|> timeIt (Array.sumBy (fun a -> calc (25, a)))
|> printfn "Part1: %i"

let calcWithCache n a = 
    let cache = Dictionary<int * int64, int64>()

    let rec calc n (a: int64 ) =
        let key = (n, a) 
        let n' = n - 1
        match cache.TryGetValue key with
        | true, cn -> cn
        | false, _ ->
            let res =
                match n, a with
                | 0, _ -> 1L 
                | _, 0L -> calc n' 1
                | _, Replaceable(x, y ) -> (calc n' x) + (calc n' y)
                | _ -> calc n' (2024L * a)
            cache[key] <- res
            res
    calc n a

data
|> timeIt (Array.sumBy (calcWithCache 75))
|> printfn "Part2: %i"
