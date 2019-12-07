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

let rec comb (n : int) (l : list<int>) =
    match n with
        | 0 -> [[]]
        | 1 -> List.map List.singleton l
        | k -> List.collect (fun x -> List.map (fun y -> y :: x) l) (comb (k - 1) l)

let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)

type Phasesetting = list<int>

let computePS ((is, os, ms): Computation) (s:int) =
    let is' = s :: is 
    let (_, os', _) = compute (is', [], Array.copy ms) 0
    (os', os', ms)

let chain (c : Computation ) (ps : Phasesetting) = 
    List.fold computePS c ps

let init ms = ([0], [], ms)
      
[<EntryPoint>]
let main argv =
    let ms = (File.ReadAllText argv.[0]).Split([|','|])
                |> Array.map int

    let ps = permute [0;1;2;3;4]
    let (_, o, _) = List.map (chain (init ms)) ps
                      |> List.maxBy (fun (_, os, _) -> List.head os)

    printfn "Output: %i" <| List.head o
    0 // return an integer exit code
