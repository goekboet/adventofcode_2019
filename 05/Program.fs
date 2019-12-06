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
    | Jit of Par * Par
    | Jif of Par * Par
    | Lt  of Par * Par * Pos
    | Eq  of Par * Par * Pos
    | Hal

let parseOpcode p (ms : Memories) = 
    let c = sprintf "%05i" (ms.[p])
    match (int c.[3..], c.[..2]) with
        | (1, modes) -> Add ( getParam (modes.[2], p + 1) ms
                            , getParam (modes.[1], p + 2) ms
                            , ms.[p + 3]
                            )
        | (2, modes) -> Mul ( getParam (modes.[2], p + 1) ms
                            , getParam (modes.[1], p + 2) ms
                            , ms.[p + 3]
                            )
        | (3, _    ) -> Inp ms.[p + 1]
        | (4, modes) -> Out ( getParam (modes.[2], p + 1) ms)
        | (5, modes) -> Jit ( getParam (modes.[2], p + 1) ms
                            , getParam (modes.[1], p + 2) ms
                            )
        | (6, modes) -> Jif ( getParam (modes.[2], p + 1) ms
                            , getParam (modes.[1], p + 2) ms
                            )
        | (7, modes) -> Lt  ( getParam (modes.[2], p + 1) ms
                            , getParam (modes.[1], p + 2) ms
                            , ms.[p + 3]
                            )
        | (8, modes) -> Eq  ( getParam (modes.[2], p + 1) ms
                            , getParam (modes.[1], p + 2) ms
                            , ms.[p + 3]
                            ) 
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
        | Jit (pred, v) ->
            if pred <> 0
            then compute (is, os, ms) v
            else compute (is, os, ms) (pt + 3)
        | Jif (pred, v) ->
            if pred = 0
            then compute (is, os, ms) v
            else compute (is, os, ms) (pt + 3)
        | Lt (p1, p2, pos) ->
            if p1 < p2
            then Array.set ms pos 1
            else Array.set ms pos 0
            compute (is, os, ms) (pt + 4)
        | Eq (p1, p2, pos) ->
            if (p1 = p2)
            then Array.set ms pos 1
            else Array.set ms pos 0
            compute (is, os, ms) (pt + 4)
        | _     -> (is, os, ms)
                      
[<EntryPoint>]
let main argv =
    let ms = (File.ReadAllText argv.[0]).Split([|','|])
                |> Array.map int

    let is = List.singleton 5
    let os = List.empty
    
    let (_, os', ms') = compute (is, os, ms) 0 
    printfn "Output:\n%s" <| String.Join ("\n", List.map string os' |> List.toArray)
    
    0 // return an integer exit code
