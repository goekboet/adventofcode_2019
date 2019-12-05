// Learn more about F# at http://fsharp.org

open System
open System.IO
open Microsoft.FSharp.Core.Operators.Checked

type Inputs = list<int>
type Outputs = list<int>
type Memories = int[]
type Computation = Inputs * Outputs * Memories

let getParam (m, pt) (ms : Memories) =
    match m with
        | '0' -> let v = ms.[pt] in ms.[v]
        | _   -> ms.[pt]

type Pos = int
type Par = int

type Opcode 
    = Add of Par * Par * Pos
    | Mul of Par * Par * Pos
    | Inp of Pos
    | Out of Pos
    | Hal

let parseOpcode p (ms : Memories) = 
    let c = sprintf "%05i" (ms.[p])
    match (int c.[3..], c.[..2]) with
        | (1, modes) -> Add ( getParam (modes.[2], p + 1) ms
                            , getParam (modes.[1], p + 2) ms
                            , ms.[p + 3])
        | (2, modes) -> Mul ( getParam (modes.[2], p + 1) ms
                            , getParam (modes.[1], p + 2) ms
                            , ms.[p + 3])
        | (3, _    ) -> Inp ms.[p + 1]
        | (4, modes) -> Out (getParam (modes.[2], p + 1) ms)
        | _          -> Hal

let rec compute (is, os, ms : Memories) pt =
    let opc = parseOpcode pt ms
    
    match opc with
        | Add (lhs, rhs, reg) ->
            Array.set ms reg (lhs + rhs)
            compute (is, os, ms) (pt + 4)
        | Mul (lhs, rhs, reg) ->
            Array.set ms reg (lhs * rhs)
            compute (is, os, ms) (pt + 4)
        | Inp p ->
            let (i, is') = (List.head is, List.tail is)
            Array.set ms p i
            compute (is', os, ms) (pt + 2)
        | Out p ->
            let os' = p :: os
            compute (is, os', ms) (pt + 2)
        | _     -> (is, os, ms)
                      
[<EntryPoint>]
let main argv =
    let ms = (File.ReadAllText argv.[0]).Split([|','|])
                |> Array.map int

    let is = List.singleton 1
    let os = List.empty
    
    let (_, os', ms') = compute (is, os, ms) 0 
    printfn "Output:\n%s" <| String.Join ("\n", List.map string os' |> List.toArray)
    
    0 // return an integer exit code
