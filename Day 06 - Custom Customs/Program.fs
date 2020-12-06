open System.IO

let rec toGroups (input: List<string>) =
    let group =
        List.takeWhile (fun line -> line <> "") input

    match List.skip (List.length group) input with
    | "" :: remainder -> group :: toGroups remainder
    | _ -> [ group ]


[<EntryPoint>]
let main argv =
    let input =
        File.ReadAllLines "input.txt" |> Seq.toList

    let groupAnswers = input |> toGroups |> List.map (List.map Set.ofSeq)

    // Part 1
    groupAnswers
    |> List.map (List.fold Set.union Set.empty)
    |> List.map Set.count
    |> List.sum
    |> printfn "%d"

    // Part 2
    groupAnswers
    |> List.map (List.fold Set.intersect <| Set.ofSeq [ 'a' .. 'z' ])
    |> List.map Set.count
    |> List.sum
    |> printfn "%d"

    0
