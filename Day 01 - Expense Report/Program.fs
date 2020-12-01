// Learn more about F# at http://fsharp.org

open System.IO

let rec findPair lookup elements =
    match elements with
    | (x :: xs) ->
        match Map.tryFind (2020 - x) lookup with
        | Some _ -> Some((x, 2020 - x))
        | None -> findPair (Map.add x true lookup) xs
    | [] -> None

let rec mkPairs lst =
    match lst with
    | (x :: xs) -> List.map (fun y -> (x, y)) xs @ mkPairs xs
    | [] -> []

[<EntryPoint>]
let main argv =
    let input =
        File.ReadLines "input.txt"
        |> Seq.toList
        |> List.map int

    input
    |> findPair Map.empty
    |> fun x ->
        printfn "%A" x
        x
    |> Option.map (fun (x, y) -> x * y)
    |> printfn "%A"

    mkPairs input
    |> List.filter (fun (x, y) -> x + y <= 2020)
    |> List.collect (fun (x, y) ->
        input
        |> List.filter (fun z -> z <> x)
        |> List.filter (fun z -> z <> y)
        |> List.map (fun z -> (x, y, z)))
    |> List.filter (fun (x, y, z) -> x + y + z = 2020)
    |> List.tryHead
    |> fun x ->
        printfn "%A" x
        x
    |> Option.map (fun (x, y, z) -> x * y * z)
    |> printfn "%A"

    0
