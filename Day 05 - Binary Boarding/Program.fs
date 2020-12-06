open System.IO

let toSeatId (code: string) =
    Seq.fold (fun seatId c ->
        match c with
        | 'F' -> seatId <<< 1
        | 'B' -> seatId <<< 1 ||| 1
        | 'L' -> seatId <<< 1
        | 'R' -> seatId <<< 1 ||| 1
        | _ -> failwith "Invalid") 0 code

[<EntryPoint>]
let main argv =
    let input =
        File.ReadAllLines "input.txt" |> Seq.map toSeatId

    let takenSeats = input |> Seq.sort |> Seq.toList

    // Part 1
    printfn "Highest Seat: %d" (List.max takenSeats)

    // Part 2
    List.pairwise takenSeats
    |> List.filter (fun (l, r) -> r - l = 2)
    |> List.map (fun (l, _) -> l + 1)
    |> List.head
    |> printfn "My Seat: %d"

    0
