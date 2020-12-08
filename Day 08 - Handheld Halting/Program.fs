open System.IO
open System.Text.RegularExpressions
open FSharp.Collections.ParallelSeq

type Instruction = { Operator: string; Operand: int }

let parseInstruction line =
    let m =
        Regex(@"^(\w{3}) (\+|-)(\d+)$").Match(line)

    match m.Success with
    | true ->
        Some
            ({ Operator = m.Groups.[1].Value
               Operand =
                   match m.Groups.[2].Value with
                   | "-" -> -1 * (m.Groups.[3].Value |> int)
                   | _ -> (m.Groups.[3].Value |> int) })
    | _ -> None

type State =
    { InstrPtr: int
      Acc: int
      Instructions: Instruction [] }

let runInstruction { InstrPtr = instrPtr; Acc = acc; Instructions = instructions }: State =
    let { Operator = operator; Operand = operand } = instructions.[instrPtr]
    //    printfn "InstrPtr: %d, Acc %d, Operator: %s, Operand: %d" instrPtr acc operator operand
    match operator with
    | "acc" ->
        { InstrPtr = instrPtr + 1
          Acc = acc + operand
          Instructions = instructions }
    | "jmp" ->
        { InstrPtr = instrPtr + operand
          Acc = acc
          Instructions = instructions }
    | _ ->
        { InstrPtr = instrPtr + 1
          Acc = acc
          Instructions = instructions }

let rec runProgram (state: State) (executedInstructions: Set<int>): Result<State, State> =
    if Set.contains state.InstrPtr executedInstructions
       || state.InstrPtr < 0 then
        Result.Error state
    else if (state.InstrPtr >= state.Instructions.Length) then
        Result.Ok state
    else
        let newState = runInstruction state
        runProgram newState (Set.add state.InstrPtr executedInstructions)

let rec variations program =
    match program with
    | { Operator = operator; Operand = operand } :: tail ->
        let remainder =
            variations tail
            |> List.map (fun vars ->
                { Operator = operator
                  Operand = operand }
                :: vars)

        match operator with
        | "nop" ->
            ({ Operator = "jmp"; Operand = operand } :: tail)
            :: remainder
        | "jmp" ->
            ({ Operator = "nop"; Operand = operand } :: tail)
            :: remainder
        | _ -> remainder
    | [] -> [ [] ]

[<EntryPoint>]
let main argv =
    let program =
        File.ReadAllLines "input.txt"
        |> Seq.map parseInstruction
        |> Seq.choose id
        |> Seq.toArray

    let initialState =
        { InstrPtr = 0
          Acc = 0
          Instructions = program }

    // Part 1
    match runProgram initialState Set.empty with
    | Error result -> result.Acc |> printfn "Loop with acc: %d"
    | Ok _ -> printfn "Failed to find loop"

    // Part 2
    let programVariations =
        program
        |> Array.toList
        |> variations
        |> List.map Array.ofList

    List.length programVariations
    |> printfn "Program Variations: %d"

    programVariations
    |> PSeq.map (fun prog ->
        runProgram
            { InstrPtr = 0
              Acc = 0
              Instructions = prog }
            Set.empty)
    |> Seq.choose (fun result ->
        match result with
        | Ok res -> Some res
        | _ -> None)
    |> Seq.head
    |> fun { Acc = acc } -> acc
    |> printfn "Acc after fix: %d"

    0
