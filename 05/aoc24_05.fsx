open System

let t = """47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"""

let test: string array = t.Split('\n')
let getRules (s: string[])  = 
    s 
    |> Array.takeWhile (_.Contains("|"))
    |> Array.map (_.Split('|') >> (fun x -> x[0], x[1]))
    |> Array.groupBy fst
    |> Array.map (fun (x, ys) -> x, ys |> Array.map snd|> set)
    |> Map.ofArray

let getPrints (s: string array) =
    s
    |> Array.skipWhile (not << _.Contains(","))
    |> Array.map _.Split(',')

let rec verifyPrint  (rules: Map<string, Set<string>>) (print: string[]) =
    match print with
    | [||] | [| _ |] -> true
    | _ ->
        let rest = print[1..]
        let rs = rules[print[0]]
        match Array.forall rs.Contains rest with
        | true -> verifyPrint rules rest
        | false -> false

let partOne (s: string array) =
    let rules = getRules s

    s
    |> getPrints
    |> Array.filter (verifyPrint rules)
    |> Array.sumBy (fun a -> int a[a.Length/2])

let partTwo (s: string array) =
    let rules = getRules s

    s
    |> getPrints
    |> Array.filter (not << verifyPrint rules)
    |> Array.map (
        Array.sortWith (fun a b ->
            if rules[a].Contains b then -1 else 1
        )
    )
    |> Array.sumBy (fun a -> int a[a.Length/2])

IO.File.ReadAllLines("input.txt")
|> partOne

IO.File.ReadAllLines("input.txt")
|> partTwo