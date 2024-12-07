open System.IO
open System.Collections.Generic

let test = """190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20""".Split('\n') 

let parse (s: string array) = s |> Array.map (_.Replace(":","").Split(" ")>> (Array.map int64))

let concat a b =
  let rec loop acc  = function
    | 0L -> acc + b
    | t -> loop (acc * 10L) (t / 10L)
  loop a b

let cache = Dictionary<int, _>()
let gen (n: int) (ops: 'b array) =
    match cache.TryGetValue n with
    | true, g -> g
    | false, _ -> 
        let y = [|
            for i in 1 ..  (pown ops.Length n)  do [|
                    for j in 0 .. n - 1 do ops[i / (pown ops.Length j)  % ops.Length ]
                |]
        |]
        cache[n] <- y
        y

let calc (n: int64 array) (a: ((int64 -> int64 -> int64) array ))  =
    n[2..]
    |> Array.fold (fun (i, acc)  curr ->
        (i + 1, a[i] acc curr)) (0, n[1])
    |> fun (_, x) -> n[0] = x
let ops1 = [|(+) ;(*)|]
let ops2 = [| (+) ;(*) ;concat |]
let handle ops (a: int64 array)  =
    gen (a.Length - 2 ) ops
    |> Array.exists (calc a)

let handler ops x =
    x
    |> Array.filter (handle ops)
    |> Array.sumBy (fun x -> x[0])

parse test
|> handler ops1
|> printfn "%A"

File.ReadAllLines("input.txt")
|> parse
|> handler ops1
|> printfn "%A"

cache.Clear()

parse test
|> handler ops1
|> printfn "%A"

File.ReadAllLines("input.txt")
|> parse
|> fun a ->
    let sw = System.Diagnostics.Stopwatch()
    sw.Start() 
    let x = handler ops2 a
    printfn "%O" sw.Elapsed
    x 
|> printfn "%A"