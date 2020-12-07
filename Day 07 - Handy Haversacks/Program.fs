// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Text.RegularExpressions

let parseRule rule =
    let m =
        Regex(@"^(\w+ \w+) bags contain (\d+) (\w+ \w+) bags?(?:, (\d+) (\w+ \w+) bags?)*.$").Match(rule)

    if m.Success then
        let outer = m.Groups.[1].Value

        let innerHead =
            (m.Groups.[2].Value |> int, m.Groups.[3].Value)

        let innerTail =
            List.zip
                (m.Groups.[4].Captures
                 |> Seq.toList
                 |> List.map (fun cap -> cap.Value |> int))
                (m.Groups.[5].Captures
                 |> Seq.toList
                 |> List.map (fun cap -> cap.Value))

        Some(outer, innerHead :: innerTail)
    else
        None

let rec ancestors (predecessors: Map<string, Set<string>>) (current: Set<string>) =
    let predecessorsOfCurrent =
        Seq.collect (fun bag ->
            Map.tryFind bag predecessors
            |> Option.defaultValue Set.empty) current

    let newCurrent =
        Set.union current
        <| Set.ofSeq predecessorsOfCurrent

    if (current = newCurrent) then current else ancestors predecessors newCurrent

let rec descendantCount (successors: Map<string, List<(int * string)>>) (bag: string): int =
    match Map.tryFind bag successors with
    | Some succs ->
        List.map (fun (cnt, succ) -> cnt * (1 + descendantCount successors succ)) succs
        |> List.sum
    | None -> 0

[<EntryPoint>]
let main argv =

    let input = File.ReadAllLines "input.txt"

    let rules =
        input
        |> Seq.toList
        |> List.map parseRule
        |> List.choose id

    // Part 1
    let predecessors =
        rules
        |> List.collect (fun (outer, inner) -> List.map (fun (_, i) -> (i, outer)) inner)
        |> List.fold (fun m (inner, outer) ->
            match Map.tryFind inner m with
            | Some outers -> Map.add inner (Set.add outer outers) m
            | None -> Map.add inner (Set.singleton outer) m) Map.empty

    let myBag = "shiny gold"

    ancestors predecessors (Set.singleton myBag)
    |> Set.remove myBag
    |> Set.count
    |> printfn "Possible bags containing %s: %d" myBag

    // Part 2
    let successors = rules |> Map.ofList

    descendantCount successors myBag
    |> printfn "Bags required inside %s: %d" myBag

    0
