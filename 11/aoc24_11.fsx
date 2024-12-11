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
    printfn "us %O ticks %i" sw.Elapsed.TotalMicroseconds sw.ElapsedTicks
    r
   
let (|Divable|_|) (x: int64) =
    let xlog10 = int <| floor (log10 (float x))
    if xlog10 % 2 = 1 then 
        let y = xlog10 / 2 + 1
        let factor = int64 <| pown 10 y
        Some <| (x / factor, x % factor)
    else None

let rec calc n (a: int64 ) = 
    if n = 0 then 1L 
    else 
        match a with
        | 0L -> calc (n - 1) 1
        | Divable(x, y ) -> (calc (n - 1) x) + (calc (n - 1) y)
        | _ -> calc (n - 1) (2024L * a )
 
data
|> timeIt (Array.sumBy (calc 25))
|> printfn "Part1: %i"

let calcWithCache n a = 
    let cache = Dictionary<int * int64, int64>()

    let rec calc n (a: int64 ) =
        let key = (n, a) 
        match cache.TryGetValue  key with
        | true, cn -> cn
        | false, _ ->
            if n = 0 then 1L 
            else 
                let res=
                    match a with
                    | 0L -> calc (n - 1) 1
                    | Divable(x, y ) -> (calc (n - 1) x) + (calc (n - 1) y)
                    | _ -> calc (n - 1) (2024L * a )
                cache[key] <- res
                res
    calc n a

data
|> timeIt (Array.sumBy (calcWithCache 75))
|> printfn "Part2: %i"
