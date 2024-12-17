open System.IO
type Instruction =
    | Adv of int64
    | Bxl of int64
    | Bst of int64
    | Jnz of int64
    | Bcx of int64
    | Out of int64
    | Bdv of int64
    | Cdv of int64
type State = {
    A: int64
    B: int64
    C: int64
    I: int
    O: int64 list
}
let input s =
    let x = File.ReadAllText($"{__SOURCE_DIRECTORY__}/%s{s}").Split('\n', System.StringSplitOptions.TrimEntries ||| System.StringSplitOptions.RemoveEmptyEntries)
    int64 <| x[0].Split(':')[1] , (x[3].Split(":")[1] |> _.Trim())
let a1, p1 = input "input1.txt"
let _a2, p2 = input "input2.txt"
let a, program = input "input.txt"
let parse (p: string) =
    let program = p.Replace(",","")
    [|
        for x in 0 .. 2 .. program.Length - 1 do
            let v = int64 (string program[x + 1])
            match program[x] with
            | '0' -> Adv v
            | '1' -> Bxl v
            | '2' -> Bst v
            | '3' -> Jnz v
            | '4' -> Bcx v
            | '5' -> Out v
            | '6' -> Bdv v
            | '7' -> Cdv v
            | _ -> failwith $"bad program{program[x]}"
    |]

let (|Combo|) state = function
    | 0L | 1L | 2L | 3L as v -> v
    | 4L -> state.A
    | 5L -> state.B
    | 6L -> state.C
    | x -> failwith $"{x}"
let (|Pown|) c = pown 2L (int c)
let run (p: Instruction array) (s: State) =
    let rec loop s = 
        if s.I >= p.Length then s
        else 
            match p[s.I] with
            | Adv (Combo s (Pown c)) -> 
                loop { s with A = s.A / c ; I = s.I + 1}
            | Bxl v ->
                loop { s with B = s.B ^^^ v ; I = s.I + 1}
            | Bst (Combo s c) ->
                loop { s with B = c % 8L ; I = s.I + 1}
            | Jnz v ->
                if s.A = 0 
                then loop { s with I = s.I + 1}
                else loop { s with I = int v / 2}
            | Bcx _ ->
                loop { s with I = s.I + 1 ; B = s.B ^^^ s.C }
            | Out (Combo s c)  ->
                loop { s with I = s.I + 1 ; O = c % 8L :: s.O }
            | Bdv (Combo s (Pown c))  ->
                loop { s with B = s.A / c ; I = s.I + 1}
            | Cdv (Combo s (Pown c))  ->
                loop { s with C = s.A / c ; I = s.I + 1}
    let r = loop s
    List.rev r.O |> List.map string |> String.concat ","

let state n = {
    A = n
    B = 0
    C = 0
    I = 0
    O = []
}

run (parse p1) (state a1) |> printfn "%s"
run (parse program) (state a) |> printfn "%s"

let run2 (program: string) =
    let p = parse program
    let rec loop (acc: int64) (pos: int64 list) (n: int64) =
        match n with
        | 8L ->
            pos
            |> Seq.choose (fun nx -> loop ((acc + nx) <<< 3) [] 0)
            |> Seq.tryHead
        | _ ->
            let x = state (acc + n) |> run p
            if program = x then Some (acc + n)
            elif program.EndsWith x then loop acc (n :: pos) (n + 1L)
            else loop acc pos ( n + 1L)
    loop 0L [] 0L

run2 p2 |> printfn "%A"
run2 program |> printfn "%A"
