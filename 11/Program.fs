// Learn more about F# at http://fsharp.org

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

type Direction = U | R | D | L
type Position = int * int
type Color = Black | White

type Robot = Position * Direction * Map<Position,Color>
let initRobot = ((0,0), U, Map.empty)

let parseDirection d t =
    match (d, t) with
        | (U, 1) -> R
        | (U, 0) -> L
        | (R, 1) -> D
        | (R, 0) -> U
        | (D, 1) -> L
        | (D, 0) -> R
        | (L, 1) -> U
        | _      -> D

let nextPos (x, y) dir =
    match dir with
        | U -> (x    ,y + 1)
        | R -> (x + 1,y    )
        | D -> (x    ,y - 1)
        | L -> (x - 1,y    )

let getPaint x = if x = 0 then Black else White
let paint = Map.add 

let getOutput (p : Position) (ps : Map<Position, Color>) =
    let c = Map.tryFind p ps
            |> Option.defaultWith ( fun _ -> Black )

    match c with
        | White -> 1
        | _     -> 0

let runrobot (inp : Inputs) (pos, dir, ps) =
    let col  = getPaint (int inp.[1])
    let dir' = parseDirection dir (int inp.[0])
    let ps'  = paint pos col ps
    let pos' = nextPos pos dir'
    let outp = getOutput pos' ps'
    
    ((pos', dir', ps'), [int64 outp])

let init (ms : Memories) (prg : int64[]) (inp : Inputs) = 
    Array.blit prg 0 ms 0 prg.Length

    (inp, [], ms, 0, initRobot )

let rec compute (is, os, ms : Memories, rb : RelativeBase, r : Robot) pt =
    let opc = parseOpcode pt ms rb
    
    match opc with
        | Add (lhs, rhs, reg) ->
            Array.set ms (int reg) (lhs + rhs)
            compute (is, os, ms, rb, r) (pt + 4)
        | Mul (lhs, rhs, reg) ->
            Array.set ms (int reg) (lhs * rhs)
            compute (is, os, ms, rb, r) (pt + 4)
        | Inp p ->
            let (i, is') = (List.head is, List.tail is)
            Array.set ms (int p) i
            compute (is', os, ms, rb, r) (pt + 2)
        | Out p ->
            let os' = p :: os
            if os'.Length = 2
            then
                let (r', is') = runrobot os' r
                compute (is', [], ms, rb, r') (pt + 2)
            else
                compute (is, os', ms, rb, r) (pt + 2)
        | Jit (pred, v) ->
            if pred <> 0L
            then compute (is, os, ms, rb, r) (int v)
            else compute (is, os, ms, rb, r) (pt + 3)
        | Jif (pred, v) ->
            if pred = 0L
            then compute (is, os, ms, rb, r) (int v)
            else compute (is, os, ms, rb, r) (pt + 3)
        | Lt (p1, p2, pos) ->
            if p1 < p2
            then Array.set ms (int pos) 1L
            else Array.set ms (int pos) 0L
            compute (is, os, ms, rb, r) (pt + 4)
        | Eq (p1, p2, pos) ->
            if (p1 = p2)
            then Array.set ms (int pos) 1L
            else Array.set ms (int pos) 0L
            compute (is, os, ms, rb, r) (pt + 4)
        | MRBase c -> compute (is, os, ms, rb + (int c), r) (pt + 2)
        | _     -> (is, os, ms, rb, r)

let normalize (x, y) (x', y') = (x' - x, y - y') 

let show (ps : Map<Position, Color>) =
    let cs = 
        ps
        |> Map.toList
        |> List.map fst

    let (right,_  ) = cs |> List.maxBy fst
    let (left ,_  ) = cs |> List.minBy fst
    let (_, top   ) = cs |> List.maxBy snd
    let (_, bottom) = cs |> List.minBy snd
    let w  = right - left
    let h = top - bottom 
    // let ps' = ps
    //           |> Map.toSeq
    //           |> Seq.map (fun ((x, y), v) -> ((x - bottom, y - left), v))
    //           |> Map.ofSeq 

    let normalize (x, y) = (x + left, y + bottom)
    let defBlack x = 
        Map.tryFind x ps
        |> Option.defaultWith (fun _ -> Black)
    let rune c =
        match c with
        | White -> '#'
        | Black -> ' ' 

    Seq.init (w * h) (fun i -> (i % w, i / w))
        |> Seq.map (rune << defBlack << normalize)
        |> Seq.chunkBySize w
        |> Seq.rev
        |> Seq.map (fun x -> new string(x))
        

    

[<EntryPoint>]
let main argv =
    let prg = (File.ReadAllText argv.[0]).Split([|','|])
                |> Array.map int64

    let ms = Array.create (1000 * 1000) 0L
    let c = init ms prg [1L]

    let (_, _, _, _, (_, _, ps)) = compute c 0

    printfn "sanity : %A" <| Map.find (0,0) ps
    printfn "painted: %i" ps.Count
    
    let output = show ps |> Seq.toList

    List.iter (printfn "%s") output
    



    0 // return an integer exit code
