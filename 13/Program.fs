// Learn more about F# at http://fsharp.org

open System
open System.IO
open Microsoft.FSharp.Core.Operators.Checked
open System.Threading

type Inputs = list<int64>
type Outputs = list<int64>
type Memories = int64[]
type RelativeBase = int

type Score = int
type Vector = int * int
type TileId
    = Empty = 0
    | Wall = 1
    | Block = 2
    | Paddle = 3
    | Ball = 4

type GameState
    = list<Map<Vector, TileId>>
    * list<Score>





type Computation = Inputs * Outputs * Memories * RelativeBase * GameState

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



type Command = Left = -1L | Noop = 0L | Right = 1L

let initGameState : GameState = ([Map.empty], [])

let changegameState ((ts, sc) : GameState) (os : list<int64>)
    =
    match os with
    | [s; 0L; -1L] -> ((ts, int s :: sc), [])
    | [tId; y; x] -> ((Map.add (int x, int y) (enum<TileId>(int tId)) (List.head ts) :: ts, sc), [])
    | _           -> ((ts, sc), os)


let getTileId 
    (tId : TileId) 
    (ts : Map<Vector,TileId>) 
    =
    Map.pick (fun p t -> if t = tId then Some p else None) ts

let lastPos
    (tId : TileId)
    (b : Vector)
    (ts : Map<Vector,TileId>)
    =
    Map.tryPick (fun p t -> if t = tId && b <> p then Some p else None) ts

let followBall ((x', _) : Vector) ((pX', _) : Vector)  
    =
    if pX' > x' then -1
    elif pX' < x' then 1
    else 0

let computeInput ((ts, _) : GameState)
    =
    let t1 = List.head ts
    let p = getTileId TileId.Paddle t1
    let b = getTileId TileId.Ball t1

    followBall b p

let init (ms : Memories) (prg : int64[]) (inp : Inputs) : Computation = 
    Array.blit prg 0 ms 0 prg.Length

    (inp, [], ms, 0, initGameState )

let rec compute (is, os, ms : Memories, rb : RelativeBase, gs : GameState) pt =
    let opc = parseOpcode pt ms rb
    
    match opc with
        | Add (lhs, rhs, reg) ->
            Array.set ms (int reg) (lhs + rhs)
            compute (is, os, ms, rb, gs) (pt + 4)
        | Mul (lhs, rhs, reg) ->
            Array.set ms (int reg) (lhs * rhs)
            compute (is, os, ms, rb, gs) (pt + 4)
        | Inp p ->
            let i = computeInput gs

            Array.set ms (int p) (int64 i)
            compute ([], os, ms, rb, gs) (pt + 2)
        | Out p ->
            let os' = p :: os
            let (gs', os'') = changegameState gs os'
            compute (is, os'', ms, rb, gs') (pt + 2)
        | Jit (pred, v) ->
            if pred <> 0L
            then compute (is, os, ms, rb, gs) (int v)
            else compute (is, os, ms, rb, gs) (pt + 3)
        | Jif (pred, v) ->
            if pred = 0L
            then compute (is, os, ms, rb, gs) (int v)
            else compute (is, os, ms, rb, gs) (pt + 3)
        | Lt (p1, p2, pos) ->
            if p1 < p2
            then Array.set ms (int pos) 1L
            else Array.set ms (int pos) 0L
            compute (is, os, ms, rb, gs) (pt + 4)
        | Eq (p1, p2, pos) ->
            if (p1 = p2)
            then Array.set ms (int pos) 1L
            else Array.set ms (int pos) 0L
            compute (is, os, ms, rb, gs) (pt + 4)
        | MRBase c -> compute (is, os, ms, rb + (int c), gs) (pt + 2)
        | _     -> (is, os, ms, rb, gs)



let screenwidth = 39
let screenHeight = 25

let showTile t
    =
    match t with
    | TileId.Empty  -> ' '
    | TileId.Wall   -> 'X'
    | TileId.Block  -> '.'
    | TileId.Paddle -> '_'
    | _             -> 'O'

let showScreenRow (ts : Map<Vector, TileId>) (y : int)
    =
    let toVector x = (x, y) 
    let lookup x = 
        Map.tryFind x ts
        |> Option.defaultValue TileId.Empty

    let toRow tIds = new string(tIds |> Array.ofSeq)

    toRow ({0 .. screenwidth} |> Seq.map (showTile << lookup << toVector))
    

let showGamestate (ts : Map<Vector, TileId>)
    =
    {0 .. screenHeight}
    |> Seq.map (showScreenRow ts)

let clearScreen w h =
    Thread.Sleep (TimeSpan.FromSeconds(0.2))
    Console.SetCursorPosition (0,0)
    Console.Clear ()

[<EntryPoint>]
let main argv =
    let prg = (File.ReadAllText argv.[0]).Split([|','|])
                |> Array.map int64
    Array.set prg 0 2L 

    let ms = Array.create (1000 * 1000) 0L

    let is = List.replicate 50000 0L
    
    let (_, _, _, _, (gs', sc)) = compute (init ms prg is) 0
    
    let debug = if (List.head sc) = 0 then Seq.rev gs' else Seq.empty
    debug 
        |> Seq.filter (Map.containsKey (screenwidth, screenHeight)) 
        |> Seq.iter 
            (fun g -> 
                ignore (clearScreen 30 40)
                let px = showGamestate g
                px |> Seq.iter (fun x -> Console.WriteLine(x)))
            
    printfn "score: %i" (List.head sc)

    0 // return an integer exit code
