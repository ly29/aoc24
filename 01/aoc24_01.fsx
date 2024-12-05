open System
open System.IO

let t = """
3   4
4   3
2   5
1   3
3   9
3   3
"""
let f = File.ReadAllText($"{__SOURCE_DIRECTORY__}/input.txt")

let parse (s: string) =
    s.Split('\n', System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun s -> s.Split(' ', StringSplitOptions.RemoveEmptyEntries))
    |> Array.map (fun a -> Int64.Parse(a[0]), Int64.Parse(a[1]))
    |> Array.unzip

let job1 (t: string) =
    let x, y = parse t

    Array.zip (Array.sort x) (Array.sort y)
    |> Array.sumBy (fun (t, u) -> abs (t - u))

let job2 (s: string) =
    let x, y = parse s
    let counted = 
        y
        |> Array.countBy id
        |> Map.ofArray

    x
    |> Array.sumBy (fun n -> 
        match counted.TryFind n with
        | Some x -> (int64 x) * n
        | None -> 0L
    )

job1 t
job1 f
job2 t
job2 f