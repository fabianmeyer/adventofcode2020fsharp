open System
open System.IO

let printSeatingArea (seatingArea: List<List<char>>) =
    seatingArea
    |> List.map (fun it -> Array.ofSeq it |> String)
    |> List.iter (printfn "%s")

let seatingAreaWithEmptyRowBorder seatingArea =
    let seatsPerRow = seatingArea |> List.head |> List.length
    let emptyRow = List.replicate seatsPerRow '.'
    emptyRow
    :: seatingArea
    @ [ emptyRow ]
    |> List.map (fun row -> '.' :: row @ [ '.' ])

let adjancents seatingArea =
    seatingAreaWithEmptyRowBorder seatingArea
    |> List.windowed 3
    |> List.map (fun [ row1; row2; row3 ] ->
        List.zip3 row1 row2 row3
        |> List.windowed 3
        |> List.map (fun [ (s11, s12, s13); (s21, s22, s23); (s31, s32, s33) ] ->
            (s22,
             [ s11
               s12
               s13
               s21
               s23
               s31
               s32
               s33 ])))

let applySeatingRules seatingArea =
    adjancents seatingArea
    |> List.map
        (List.map (fun (seat, adjs) ->
            let occupiedAdjancents =
                adjs
                |> List.filter (fun it -> it = '#')
                |> List.length

            match seat with
            | '#' -> if (occupiedAdjancents >= 4) then 'L' else '#'
            | 'L' -> if (occupiedAdjancents = 0) then '#' else 'L'
            | c -> c))

let countOccupiedSeats seatingArea =
    seatingArea
    |> List.collect (List.filter (fun seat -> seat = '#'))
    |> List.length



[<EntryPoint>]
let main argv =
    let seatingArea =
        File.ReadAllLines "input.txt"
        |> Seq.toList
        |> List.map Seq.toList

    let (iterations, result) =
        Seq.initInfinite id
        |> Seq.scan (fun s _ -> applySeatingRules s) seatingArea
        |> Seq.pairwise
        |> Seq.takeWhile (fun (a, b) -> (a <> b))
        |> fun res -> (Seq.length res, Seq.last res |> snd)


    printfn "Occupied seats at fixpoint (%d iterations): %d" iterations (countOccupiedSeats result)

    0
