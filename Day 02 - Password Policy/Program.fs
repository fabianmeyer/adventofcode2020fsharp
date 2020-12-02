open System.IO
open System.Text.RegularExpressions

type Policy = { Min: int; Max: int; Character: char }

type Input = { Policy: Policy; Password: string }

let (|ParseRegex|_|) regex str =
    let m = Regex(regex).Match(str)
    if m.Success
    then Some(List.tail [ for x in m.Groups -> x.Value ])
    else None

let parseInput input =
    match input with
    | ParseRegex @"^(\d+)-(\d+)\s(.):\s(.*)$" [ min; max; character; password ] ->
        Some
            { Policy =
                  { Min = min |> int
                    Max = max |> int
                    Character = character.[0] }
              Password = password }
    | _ -> None

let isSledRentalValid { Policy = { Min = min; Max = max; Character = character }; Password = password } =
    String.filter (fun c -> c = character) password
    |> String.length
    |> fun cnt -> min <= cnt && cnt <= max

let isTobogganValid { Policy = { Min = min; Max = max; Character = character }; Password = password } =
    let fst = password.[min - 1]
    let snd = password.[max - 1]
    (fst = character) <> (snd = character)

[<EntryPoint>]
let main argv =
    // Read input
    let input =
        File.ReadLines "input.txt"
        |> Seq.map parseInput
        |> Seq.choose id

    // Part 1
    input
    |> Seq.filter isSledRentalValid
    |> Seq.length
    |> printfn "%d"

    // Part 2
    input
    |> Seq.filter isTobogganValid
    |> Seq.length
    |> printfn "%d"

    0
