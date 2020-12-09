open System.IO

let rec permutations input =
    match input with
    | (x :: xs) -> List.map (fun y -> (x, y)) xs @ permutations xs
    | [] -> []

let findInvalidNumber numbers =
    numbers
    |> Seq.windowed 26
    |> Seq.choose (fun window ->
        let number = window |> Seq.last
        let previous = window |> Seq.take 25 |> Seq.toList

        let sums =
            permutations previous
            |> List.map (fun (x, y) -> x + y)

        if (List.contains number sums) then None else Some(number))
    |> Seq.tryHead

let rec findSequence (number: int64) (numbers: List<int64>): Option<List<int64>> =
    match numbers with
    | [] -> None
    | [ _ ] -> None
    | [ fst; snd ] -> if (fst + snd = number) then Some([ fst; snd ]) else None
    | fst :: snd :: tail ->
        Seq.scan (fun subseq num -> num :: subseq) [ fst; snd ] tail
        |> Seq.takeWhile (fun subseq -> List.sum subseq <= number)
        |> Seq.tryLast
        |> Option.filter (fun subseq -> List.sum subseq = number)
        |> Option.orElse (snd :: tail |> findSequence number)

[<EntryPoint>]
let main argv =
    let numbers =
        File.ReadAllLines "input.txt" |> Seq.map int64

    // Part 1
    let maybeInvalidNumber = findInvalidNumber numbers
    match maybeInvalidNumber with
    | Some (invalidNumber) -> printfn "Invalid number: %d" invalidNumber
    | None -> printfn "No invalid number found"

    // Part 2
    let maybeEncryptionWeakness =
        Option.bind (fun invalidNumber ->
            numbers
            |> Seq.toList
            |> findSequence invalidNumber
            |> Option.map (fun subseq -> List.min subseq + List.max subseq)) maybeInvalidNumber

    match maybeEncryptionWeakness with
    | Some (encryptionWeakness) -> printfn "Encryption Weakness: %d" encryptionWeakness
    | None -> printfn "No encryption weakness found"

    0
