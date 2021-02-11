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


type TileId = { Row: int; Col: int }

type Tile =
    { State: char
      Nw: TileId
      N: TileId
      Ne: TileId
      E: TileId
      Se: TileId
      S: TileId
      Sw: TileId
      W: TileId }

let seats seatingArea =
    seatingArea
    |> List.mapi (fun rowIdx ->
        List.mapi (fun colIdx state ->
            { State = state
              Nw = { Row = rowIdx - 1; Col = colIdx - 1 }
              N = { Row = rowIdx - 1; Col = colIdx }
              Ne = { Row = rowIdx - 1; Col = colIdx + 1 }
              E = { Row = rowIdx; Col = colIdx + 1 }
              Se = { Row = rowIdx + 1; Col = colIdx + 1 }
              S = { Row = rowIdx + 1; Col = colIdx }
              Sw = { Row = rowIdx + 1; Col = colIdx - 1 }
              W = { Row = rowIdx; Col = colIdx - 1 } }))


let adjancents seatingArea =
    seatingAreaWithEmptyRowBorder seatingArea
    |> List.windowed 3
    |> List.mapi (fun row [ row1; row2; row3 ] ->
        List.zip3 row1 row2 row3
        |> List.windowed 3
        |> List.mapi (fun col [ (s11, s12, s13); (s21, s22, s23); (s31, s32, s33) ] ->
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
    seats seatingArea
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
