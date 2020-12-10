open System.IO

let rec successors adapters =
    match adapters with
    | [] -> []
    | [ adapter ] -> [ (adapter, []) ]
    | adapter :: remainder ->
        (adapter, List.takeWhile (fun it -> it - adapter <= 3) remainder)
        :: successors remainder

let foldDistinctWays (adapter: int, succs: List<int>) (successors: Map<int, int64>): Map<int, int64> =
    match succs with
    | [] -> 1L
    | _ ->
        succs
        |> List.map (fun succ -> Map.find succ successors)
        |> List.sum
    |> fun it -> Map.add adapter it successors

[<EntryPoint>]
let main argv =
    let adapters =
        File.ReadAllLines "input.txt" |> Seq.map int

    // Part 1
    let adapterWithOutletAndDevice =
        Seq.append adapters [ 0; Seq.max adapters + 3 ]
        |> Seq.sort

    let differences =
        Seq.pairwise adapterWithOutletAndDevice
        |> Seq.map (fun (x, y) -> y - x)

    differences
    |> Seq.groupBy id
    |> Seq.map (fun (k, v) -> (k, Seq.length v))
    |> fun seq -> (Seq.find (fun (k, _) -> k = 1) seq, Seq.find (fun (k, _) -> k = 3) seq)
    |> fun ((_, ones), (_, threes)) -> printfn "1-jolt differences multiplied by 3-jolt differences: %d" (ones * threes)

    // Part 2
    let adapterSuccessors =
        adapterWithOutletAndDevice
        |> Seq.toList
        |> successors

    let distinctWays =
        List.foldBack foldDistinctWays adapterSuccessors Map.empty
        |> Map.find 0


    printfn "number of distinct ways you can arrange the adapters: %d" distinctWays

    0
