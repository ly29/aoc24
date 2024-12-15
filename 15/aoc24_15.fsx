open System
let test = """##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^""".Split('\n', StringSplitOptions.RemoveEmptyEntries)
let data = IO.File.ReadAllText($"{__SOURCE_DIRECTORY__}/input.txt").Split('\n', StringSplitOptions.RemoveEmptyEntries)
let board (i: string array ) =  i |> Array.takeWhile _.Contains("#") |> array2D
let board2 (i: string array) =
    i
    |> Array.map 
        _.Replace("#", "##")
         .Replace("O", "[]")
         .Replace(".", "..")
         .Replace("@", "@.")
    |> board

let moves (i: string array ) = i |> Array.skipWhile _.Contains("#") |> Array.collect Array.ofSeq |> List.ofSeq

let dir (ch: char) =
    printfn "%A" ch
    match ch with 
    | '<' -> 0, -1
    | '>' -> 0, 1
    | 'v' -> 1, 0
    | '^' -> -1, 0
    | x -> failwith $"got {x}"
let add (x, y) (dx, dy) = (x + dx), (y + dy)
let findStart (b: char array2d) =
    let rec loop x y : int * int =
        if y = b.GetLength(1) then loop (x + 1) 0
        elif x = b.GetLength(0) then -1, -1 
        elif b[x, y] = '@' then x, y 
        else loop x (y + 1)
    let x, y = loop 0 0
    b[x, y] <- '.'
    x, y, b
let rec move (b: char array2d) (m: char list) ((x, y) as curr) =
    printfn "%A" curr
    let rec findMaxBox xy dxy =
        let (x, y) as next = add xy dxy 
        match b[x, y] with
        | 'O' -> findMaxBox next dxy
        | _ -> xy

    match m with
    | [] -> b
    | h :: t ->
        let d = dir h
        let (x', y') as next = add curr d
        match b[x', y'] with
        | '#' -> move b t curr
        | '.' -> move b t next
        | 'O' -> 
            let lastBox = findMaxBox next d
            let (ax, by ) = add lastBox d
            match b[ax, by] with
            | '#' -> move b t curr
            | '.' -> 
                b[ax, by] <- 'O'
                b[x', y'] <- '.'
                move b t next
            | ch -> failwith $"ab {ch}"
        | ch -> failwith $"boarrd {ch}"

type BoxTree =
    | Leaf of (int * int)
    | Node of (int * int * BoxTree * BoxTree) 
let rec move2 (b: char array2d) (m: char list) ((x, y) as curr) =
    printfn "%A" curr
    let rec findMaxBox xy dxy =
        let (x, y) as next = add xy dxy 
        match b[x, y] with
        | '['| ']' -> findMaxBox next dxy
        | _ -> xy
    let rec findMaxBoxTree xy dxy =
        let (x, y) as next = add xy dxy 
        match b[x, y] with
        | '[' -> 
            let (x', y') = x + 1, y
            [x, y ; x', y'] 
            |> List.collect (fun xy -> findMaxBoxTree xy dxy)
        | ']' ->
            let (x', y') = x - 1, y
            [x, y ; x', y'] 
            |> List.collect (fun xy -> findMaxBoxTree xy dxy)
        | _ -> []
    match m with
    | [] -> b
    | h :: t ->
        let (dx, dy) as d = dir h
        let (x', y') as next = add curr d
        match b[x', y'] with
        | '#' -> move b t curr
        | '.' -> move b t next
        | '[' | ']' -> 
            match h with
            | '<' | '>' ->
                let lastBox = findMaxBox next d
                let (ax, by ) = add lastBox d
                match b[ax, by] with
                | '#' -> move b t curr
                | '.' -> 
                    for j in ax + dx .. -dx .. x' do 
                        b[j, by] <- b[j + 1, by]
                    b[x', y'] <- '.'
                    move b t next
                | ch -> failwith $"ab {ch}"
            | '^' | 'v' -> failwith "todo"
            | m -> failwith $"illegal move {m}"
        | ch -> failwith $"boarrd {ch}"
let calcSum (b: char array2d) =
    let rec loop x y acc =
        if y = b.GetLength(1) then loop (x + 1) 0 acc
        elif x = b.GetLength(0) then acc
        elif b[x, y] = 'O' then loop x (y + 1) (acc + 100 * x + y)
        else loop x (y + 1) acc
    loop 0 0 0

let part1 (s: string array) =
    let x, y, b = board s |> findStart
    let m = moves s
    move b m (x, y)
    |> calcSum

// part1 test
// part1 data

let b' = board2 test 
b' 
