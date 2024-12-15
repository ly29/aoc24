let test = "2333133121414131402"
let data = System.IO.File.ReadAllText($"{__SOURCE_DIRECTORY__}/input.txt")
let disk (d: string)  = [|
    for i in 0 .. d.Length - 1 do
        if i % 2 = 0 then
            yield! Array.create (int $"{d[i]}") (int64 ( i / 2))
        else
            yield! Array.create (int $"{d[i]}") -1L
|]

let find (a: int64 array) f index =
    let rec loop i =
        if i = a.Length then a.Length
        elif f a[i] then i
        else loop (i + 1) 
    loop index

let findBack (a: int64 array) f index =
    let rec loop i =
        if i = -1 then -1
        elif f a[i] then i
        else loop (i - 1) 
    loop index

let swap (a: _ array) n m =
    let x = a[n]
    a[n] <- a[m]
    a[m] <- x
    
let transform (a: int64 array) =
    let rec loop l h =
        let l' = find a ((=) -1) l
        let h' = findBack a ((<>) -1) h
        if h' < l' then a
        else
            swap a h' l'
            loop l' h'
    loop 0 (a.Length - 1)

let transform2 (a: int64 array) =
    let move h =
        let h' = findBack a ((=) h) (a.Length - 1)
        let h'' = 1 + findBack a ((<>) h) h'
        let rec loop  l  =
            let l' = find a ((=) -1) l
            let l'' = find a ((<>) -1) l' - 1

            if l'' >= h'' then ()
            elif (l'' -  l') >= (h' - h'') then
                for n in 0 .. (h' - h'') do
                    swap a (l' + n) (h'' + n)
            else loop (l'' + 1)
        loop 0

    [| int  <| Array.max a .. - 1 .. 0 |]  
    |> Array.iter (fun t -> move t)
    a

let checksum (a: int64 array) =
    let rec loop l acc =
        if a.Length = l then acc
        elif a[l] = -1 then loop (l + 1) acc
        else 
            loop (l + 1) (acc + (int64 l) * a[l])
    loop 0 0L

test
|> disk
|> transform
|> checksum
|> printfn "T1: %A"

data
|> disk
|> transform
|> checksum
|> printfn "D1: %A"


test
|> disk
|> transform2
|> checksum
|> printfn "T2: %A"

data
|> disk
|> transform2
|> checksum
|> printfn "D2: %A"

