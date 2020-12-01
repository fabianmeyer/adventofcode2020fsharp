open System.IO

// Find first matching pair using lookup map 
let rec findPair year lookup input =
    match input with
    | (x :: xs) ->
        match Map.tryFind (year - x) lookup with
        | Some y -> Some((x, y))
        | None -> findPair year (Map.add x x lookup) xs
    | [] -> None


// Iterate all permutation pairs 
let rec permutations input =
    match input with
    | (x :: xs) -> List.map (fun y -> (x, y)) xs @ permutations xs
    | [] -> []

// Find first matching pair using permutations
let findTriple year input =
    permutations input
    |> List.filter (fun (x, y) -> x + y <= year)
    |> List.collect (fun (x, y) ->
        input
        |> List.filter (fun z -> z <> x)
        |> List.filter (fun z -> z <> y)
        |> List.map (fun z -> (x, y, z)))
    |> List.tryFind (fun (x, y, z) -> x + y + z = year)

// Print intermediate result and return it
let printEffect x =
    printfn "%A" x
    x

[<EntryPoint>]
let main argv =
    let year = 2020

    // Read input
    let input =
        File.ReadLines "input.txt"
        |> Seq.toList
        |> List.map int

    // Part 1
    input
    |> findPair year Map.empty
    |> printEffect
    |> Option.map (fun (x, y) -> x * y)
    |> printfn "%A"

    // Part 2
    input
    |> findTriple year
    |> printEffect
    |> Option.map (fun (x, y, z) -> x * y * z)
    |> printfn "%A"

    0
