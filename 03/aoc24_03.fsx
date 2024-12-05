open System.Text.RegularExpressions
open System.IO

let data = File.ReadAllText("input.txt")

let reg = Regex("""mul\((\d+),(\d+)\)""",RegexOptions.Multiline)
[
    for r in reg.Matches(data) do
        [
            int64 r.Groups[1].Captures[0].Value 
            int64 r.Groups[2].Captures[0].Value
        ]
]
|> List.sumBy (List.reduce (*))
|> printfn "%i"

type Op =
    | Mul of int64 * int64
    | Do
    | Dont

let reg2 = Regex("""do(.)|mul\((\d+),(\d+)\)""",RegexOptions.Multiline)
[
    for r in reg2.Matches(data) do
        match r.Groups[0].Value with 
        | "don" -> Dont
        | "do(" -> Do
        | _ -> Mul(int64 r.Groups[2].Captures[0].Value, int64 r.Groups[3].Captures[0].Value)
]
|> List.fold (fun (state, acc) ->
    function
    | Do -> (true, acc)
    | Dont -> (false, acc)
    | Mul(n, m) when state -> (state, acc + m * n)
    | _ ->  (state, acc)
) (true, 0L)
|> printfn "%O"

