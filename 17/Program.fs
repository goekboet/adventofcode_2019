﻿// Learn more about F# at http://fsharp.org

open System
open System.IO
open Microsoft.FSharp.Core.Operators.Checked

type Inputs = list<int64>
type Outputs = list<int64>
type Memories = int64[]
type RelativeBase = int
type Computation = Inputs * Outputs * Memories * RelativeBase

let readParam (m, pt) (ms : Memories) (rb : RelativeBase) =
    match m with
        | '0' -> let v = ms.[pt] in ms.[int v]
        | '2' -> let v = ms.[pt] in ms.[int v + rb]
        | _   -> ms.[pt]

let writeParam (m, pt) (ms : Memories) (rb : RelativeBase) =
    match m with
        | '2' -> let v = ms.[pt] in int v + rb
        | _ -> int ms.[pt]

type Pos = int
type Par = int64

type Opcode 
    = Add of Par * Par * Pos
    | Mul of Par * Par * Pos
    | Inp of Pos
    | Out of Par
    | Jit of Par * Par
    | Jif of Par * Par
    | Lt  of Par * Par * Pos
    | Eq  of Par * Par * Pos
    | MRBase of Par
    | Hal

let parseOpcode p (ms : Memories) (rb : RelativeBase) = 
    let c = sprintf "%05i" (ms.[p])
    match (int c.[3..], c.[..2]) with
        | (1, modes) -> Add ( readParam (modes.[2], p + 1) ms rb
                            , readParam (modes.[1], p + 2) ms rb
                            , writeParam (modes.[0], p + 3) ms rb
                            )
        | (2, modes) -> Mul ( readParam (modes.[2], p + 1) ms rb
                            , readParam (modes.[1], p + 2) ms rb
                            , writeParam (modes.[0], p + 3) ms rb
                            )
        | (3, modes) -> Inp ( writeParam (modes.[2], p + 1) ms rb)
        | (4, modes) -> Out ( readParam (modes.[2], p + 1) ms rb)
        | (5, modes) -> Jit ( readParam (modes.[2], p + 1) ms rb
                            , readParam (modes.[1], p + 2) ms rb
                            )
        | (6, modes) -> Jif ( readParam (modes.[2], p + 1) ms rb
                            , readParam (modes.[1], p + 2) ms rb
                            )
        | (7, modes) -> Lt  ( readParam (modes.[2], p + 1) ms rb
                            , readParam (modes.[1], p + 2) ms rb
                            , writeParam (modes.[0], p + 3) ms rb
                            )
        | (8, modes) -> Eq  ( readParam (modes.[2], p + 1) ms rb
                            , readParam (modes.[1], p + 2) ms rb
                            , writeParam (modes.[0], p + 3) ms rb
                            )
        | (9, modes) -> MRBase ( readParam (modes.[2], p + 1) ms rb) 
        | _          -> Hal

let rec compute (is, os, ms : Memories, rb : RelativeBase) pt =
    let opc = parseOpcode pt ms rb
    
    match opc with
        | Add (lhs, rhs, reg) ->
            Array.set ms (int reg) (lhs + rhs)
            compute (is, os, ms, rb) (pt + 4)
        | Mul (lhs, rhs, reg) ->
            Array.set ms (int reg) (lhs * rhs)
            compute (is, os, ms, rb) (pt + 4)
        | Inp p ->
            let (i, is') = (List.head is, List.tail is)
            Array.set ms (int p) i
            compute (is', os, ms, rb) (pt + 2)
        | Out p ->
            let os' = p :: os
            compute (is, os', ms, rb) (pt + 2)
        | Jit (pred, v) ->
            if pred <> 0L
            then compute (is, os, ms, rb) (int v)
            else compute (is, os, ms, rb) (pt + 3)
        | Jif (pred, v) ->
            if pred = 0L
            then compute (is, os, ms, rb) (int v)
            else compute (is, os, ms, rb) (pt + 3)
        | Lt (p1, p2, pos) ->
            if p1 < p2
            then Array.set ms (int pos) 1L
            else Array.set ms (int pos) 0L
            compute (is, os, ms, rb) (pt + 4)
        | Eq (p1, p2, pos) ->
            if (p1 = p2)
            then Array.set ms (int pos) 1L
            else Array.set ms (int pos) 0L
            compute (is, os, ms, rb) (pt + 4)
        | MRBase c -> compute (is, os, ms, rb + (int c)) (pt + 2)
        | _     -> (is, os, ms, rb)

let init (ms : Memories) (prg : int64[]) (inp : Inputs) = 
    Array.blit prg 0 ms 0 prg.Length

    (inp, [], ms, 0 )

let showOutput (os : list<int64>)
    =
    let s = List.map char os |> List.toArray
    
    String(s)

type Vector = (int * int)

let model (cs : list<Char>) : Map<Vector, Char>
    =
    let row c = c <> '\n'
    let w =
        List.skipWhile (not << row) cs
        |> List.takeWhile row
        |> List.length 
    
    List.filter row cs
    |> List.mapi (fun i c -> ((i % w), i / w), c)
    |> Map.ofList

let excludeBorder (model : Map<Vector, Char>) : list<Vector> 
    =
    let vs = 
        Map.toList model
        |> List.map fst
    
    let w = List.maxBy fst vs |> fst
    let h = List.maxBy snd vs |> snd

    let border (x, y)
        =
        x = 0 || x = w || y = 0 || y = h 

    List.filter (not << border) vs
     
let isIntersection (model : Map<Vector, Char> ) ((x, y) : Vector)
    =
    let c = Map.find (x    , y    ) model
    let n = Map.find (x    , y + 1) model
    let e = Map.find (x + 1, y    ) model
    let s = Map.find (x    , y - 1) model
    let w = Map.find (x - 1, y    ) model
    
    c = '#' && n = '#' && e = '#' && s = '#' && w = '#'          

[<EntryPoint>]
let main argv =
    let prg = (File.ReadAllText argv.[0]).Split([|','|])
                |> Array.map int64

    let ms = Array.create (1000 * 1000) 0L

    let is = List.singleton 2L
    
    let (_, os', ms', _) = compute (init ms prg is) 0 
    
    let m = model (os' |> List.rev |> List.map char)
    let r =
        excludeBorder m
        |> List.filter (isIntersection m)
        
    
    printfn "%s" <| showOutput os'
    printfn "\n\nintersection count: %i" (r |> List.length)
    printfn "result: %i" (r |> List.sumBy (fun (x, y) -> x * y))
    
    0
