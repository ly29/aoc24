open System.IO

let test = """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9""".Split('\n')

let parse (s: string[]) =
    s
    |> Array.map (_.Split(" ") >> Array.map int)

let classify (x:  int[]) =
    x
    |> List.ofArray
    |> List.pairwise
    |> List.map (fun (x, y) -> x - y)
    |> function
        | (h :: _) as x when h > 0 -> 
            (x  |> List.forall (fun x -> x > 0 && x < 4))
        | (h :: _) as x when h < 0 -> 
            (x  |> List.forall (fun x -> x < 0 && x > -4))
        | _ -> false

let partOne = Array.filter classify >> Array.length

let generate (x: int[]) =
    Array.mapi (fun n _ -> Array.removeAt n x) x

let partTwo x  =
    x
    |> Array.map generate
    |> Array.filter (Array.exists classify)
    |> Array.length

test
|> parse
|> partOne

File.ReadAllLines("input.txt")
|> parse
|> partOne

test
|> parse
|> partTwo

File.ReadAllLines("input.txt")
|> parse
|> partTwo