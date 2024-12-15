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
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^""".Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)

let test' = """#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^""".Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
let data = IO.File.ReadAllText($"{__SOURCE_DIRECTORY__}/input.txt").Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)

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

let dir = function 
    | '<' -> 0, -1
    | '>' -> 0, 1
    | 'v' -> 1, 0
    | '^' -> -1, 0
    | x -> failwith $"got illegal move {x}"
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

let treePrint  (xy: (int * int) option)  (ch: char array2d)=
    for x in 0 .. ch.GetLength(0) - 1 do
        for y in 0 .. ch.GetLength(1) - 1 do
            match xy with
            | Some (x', y') when x' = x && y = y' -> printf "%c" '@'
            | _ ->
                printf "%c" ch[x, y]
        printfn ""
    ch

let rec move2 (b: char array2d) (m: char list) ( curr) =   
    let rec findMaxBox xy dxy =
        let (x, y) as next = add xy dxy 
        match b[x, y] with
        | '['| ']' -> findMaxBox next dxy
        | _ -> xy
    let classify y = function
        | '[' -> [ y; y + 1] 
        | ']' -> [ y - 1; y] 
        | _ -> []
    let rec findBoxes (dir: int) (x: int) (boxes: int list) =
        let x' = x + dir 
        let moveBoxes boxes =
            boxes |> List.iter (fun y -> b[x', y] <- b[x, y])
            boxes |> List.iter (fun y -> b[x, y] <- '.')
            true
        let openspace = boxes |> List.forall (fun y -> b[x', y] = '.')
        let wall = boxes |> List.exists (fun y -> b[x', y] = '#')
        if openspace then moveBoxes boxes
        elif wall then false 
        else 
            boxes 
            |> List.collect (fun y -> classify y b[x', y])
            |> List.distinct
            |> findBoxes dir x'
            |> function 
                | true -> moveBoxes boxes
                | false -> false           

    match m with
    | [] -> b
    | h :: t ->
        let (dx, dy) as d = dir h
        let (x', y') as next = add curr d
        match b[x', y'] with
        | '#' -> move2 b t curr
        | '.' -> move2 b t next
        | '[' | ']' as bch -> 
            match h with
            | '<' | '>' ->
                let (_lx, ly) as lastBox = findMaxBox next d
                let (ax, by ) = add lastBox d
                match b[ax, by] with
                | '#' -> move2 b t curr
                | '.' -> 
                    for j in ly .. -dy .. y' do 
                        b[ax, j + dy] <- b[ax, j]
                    b[x', y'] <- '.'
                    move2 b t next
                | ch -> failwith $"ab {ch}"
            | '^' | 'v' ->
                 classify y' bch
                |> findBoxes dx x' 
                |> function
                    | true -> move2 b t next
                    | false -> move2 b t curr
            | m -> failwith $"illegal move {m}"
        | ch -> failwith $"bad board '{ch}'"
let calcSum ch (b: char array2d) =
    let rec loop x y acc =
        if y = b.GetLength(1) then loop (x + 1) 0 acc
        elif x = b.GetLength(0) then acc
        elif b[x, y] = ch then loop x (y + 1) (acc + 100 * x + y)
        else loop x (y + 1) acc
    loop 0 0 0

let part1 (s: string array) =
    let x, y, b = board s |> findStart
    let m = moves s
    move b m (x, y)
    |> calcSum 'O'

part1 test
part1 data

let part2 (s: string array) =
    let x, y, b = board2 s |> treePrint None |> findStart
    let m = moves s
    move2 b m (x, y)
    |> treePrint None
    |> calcSum '['

part2 test
|> printfn "%A"

part2 data
|> printfn "%A"
