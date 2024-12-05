open System.IO

let data: string array =  File.ReadAllLines("input.txt")

let text = """
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX""".Split('\n', System.StringSplitOptions.RemoveEmptyEntries)

let input: char array array = text |> Array.map (Array.ofSeq)

let deltas = [
    for x in -1 .. 1 do
        for y in -1 .. 1 do
            if not (x = 0 && y = 0 ) then x, y
]

let xs =[
    for x in 0 .. input.Length - 1 do
        for y in 0 .. input[0].Length - 1  do
            if input[x][y] = 'X' then    
                x, y
]

let get (x, y) =
    Array.tryItem x input 
    |> Option.bind (Array.tryItem y)

let word = ['X'; 'M'; 'A'; 'S']
let rec matchWord (dx, dy) (x, y) =    
    function
    | [] -> true
    | h :: t -> 
        match get (x, y) with
        | Some c when c = h ->
            matchWord (dx, dy) (x + dx, y + dy) t
        | _ -> false

xs
|> List.collect (fun start ->
    deltas 
    |> List.choose (fun d ->
        if matchWord d  start word then Some start else None
    )
)
|> List.length


let m =
    function
    | [ Some 'M' ; Some 'S'] -> true
    | [ Some 'S' ; Some 'M'] -> true
    | _ -> false

let matchPattern( (x, y ): int * int) =
    match get (x, y) with
    | Some 'A' ->
            (m [ get (x - 1, y - 1); get (x + 1, y + 1) ])
            && (m [ get (x - 1, y + 1); get (x + 1, y - 1) ])
    | _ -> false

let xs2 = [
    for x in 0 .. input.Length - 1 do
        for y in 0 .. input[0].Length - 1  do
            if matchPattern (x, y) then    
                x, y
]

List.length xs2
