open System.IO
let t = """89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732""" 

let data = System.IO.File.ReadAllText($"{__SOURCE_DIRECTORY__}/input.txt")

let parse (t: string) = 
    t 
    |> _.Split('\n') 
    |> Array.map (Array.ofSeq >> Array.map (string >> int))

let tryGet t (x, y) =
    Array.tryItem x t
    |> Option.bind (Array.tryItem y)
let deltas = [ 0, 1 ; 1, 0 ; -1, 0 ; 0, -1 ]

let rec find a curr  (x, y) =
    match curr with
    | 9 ->  [ x, y ]
    | curr ->
        deltas
        |> List.choose (fun (dx, dy) ->
            let next = x - dx, y - dy
            tryGet a next
            |> Option.bind (
                fun n ->
                    if n = curr + 1 then Some( find a n next)
                    else None))
        |> List.collect id
        
let run (a: int array array) =
    [
        for x in 0  ..  a.Length - 1 do
            for y in 0 .. a[0].Length - 1 do 
                if a[x][y] = 0 then
                    find a  0  (x, y)
    ]

# 1
run (parse data)
|> List.map (List.distinct >> List.length)
|> List.sum

# 2
run (parse data)
|> List.map (List.length)
|> List.sum