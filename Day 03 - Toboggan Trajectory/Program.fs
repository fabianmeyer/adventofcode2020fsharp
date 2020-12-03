open System.IO

type Slope = { Right: int; Down: int }

let foldFn right (col, trees) (row: string) =
    (col + right, (if row.[(col - 1) % row.Length] = '#' then trees + 1L else trees))

let traverseMap (input: seq<string>) ({ Right = right; Down = down }: Slope) =
    input
    |> Seq.chunkBySize down
    |> Seq.map (Seq.head)
    |> Seq.fold (foldFn right) (1, 0L)
    |> snd

let slopes =
    [ { Right = 1; Down = 1 }
      { Right = 3; Down = 1 }
      { Right = 5; Down = 1 }
      { Right = 7; Down = 1 }
      { Right = 1; Down = 2 } ]

[<EntryPoint>]
let main argv =
    // Read input
    let input = File.ReadLines "input.txt"

    slopes
    |> Seq.map (traverseMap input)
    |> Seq.fold ((*)) 1L
    |> printfn "%d"

    0
