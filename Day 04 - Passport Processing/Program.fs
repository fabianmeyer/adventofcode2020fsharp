open System.IO
open System.Text.RegularExpressions

let rec splitInput (lines: list<string>): list<string> =
    let passEntries =
        lines |> List.takeWhile (fun s -> s.Length > 0)

    let passEntry = String.concat " " passEntries

    match List.skip (List.length passEntries) lines with
    | "" :: remainder -> passEntry :: splitInput remainder
    | _ -> [ passEntry ]

let addMapEntry map entry =
    match entry with
    | [ key; value ] -> Map.add key value map
    | _ -> map

let parseInput (line: string): Map<string, string> =
    Regex.Split(line, "\s+")
    |> Seq.map (fun entry -> entry.Split(":") |> Seq.toList)
    |> Seq.fold addMapEntry Map.empty

let ParseRegex regex str =
    let m = Regex(regex).Match(str)
    if m.Success
    then Some(List.tail [ for x in m.Groups -> x.Value ])
    else None

let validateYear field min max (data: Map<string, string>) =
    match data.TryFind field |> Option.bind (fun v -> ParseRegex @"^(\d{4})$" v) with
    | Some ([yearStr]) -> yearStr |> int |> fun year ->  min <= year && year <= max
    | _ -> false
    
let validateHeight (data: Map<string, string>) =
    match data.TryFind "hgt" |> Option.bind (fun v -> ParseRegex @"^(\d+)(cm|in)$" v) with
    | Some ([heightStr; "cm"]) -> heightStr |> int |> fun height -> 150 <= height && height <= 193
    | Some ([heightStr; "in"]) -> heightStr |> int |> fun height ->  59 <= height && height <= 76
    | _ -> false
    
let validateHairColor (data: Map<string, string>) =
    match data.TryFind "hcl" |> Option.bind (fun v -> ParseRegex @"^#[0-9a-f]{6}$" v) with
    | Some _ -> true
    | _ -> false

let validateEyeColor (data: Map<string, string>) =
    match data.TryFind "ecl" |> Option.bind (fun v -> ParseRegex @"^(amb|blu|brn|gry|grn|hzl|oth)$" v) with
    | Some _ -> true
    | _ -> false

let validatePassportId (data: Map<string, string>) =
    match data.TryFind "pid" |> Option.bind (fun v -> ParseRegex @"^\d{9}$" v) with
    | Some _ -> true
    | _ -> false

let validations =
    [ validateYear "byr" 1920 2002
      validateYear "iyr" 2010 2020
      validateYear "eyr" 2020 2030
      validateHeight
      validateHairColor
      validateEyeColor
      validatePassportId ]

let validPassport (data: Map<string, string>) =
    validations
    |> Seq.forall (fun validation -> validation data)

[<EntryPoint>]
let main argv =
    let input =
        File.ReadAllLines "input.txt"
        |> Seq.toList
        |> splitInput
        |> Seq.map parseInput

    input
    |> Seq.filter validPassport
    |> Seq.length
    |> printfn "%d"
    
    0
