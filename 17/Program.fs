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
let add ((x, y) : Vector) ((x', y') : Vector)
    =
    (x + x', y + y')

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

let part1 (m : Map<Vector,Char>)
    =
    excludeBorder m
    |> List.filter (isIntersection m)

type Direction = N | E | S | W
type Turn = L | R
type Instruction = Turn * int
let fwd ((t, s) : Instruction) = (t, s + 1) 
let showPath ((t, s) : Instruction)
    = sprintf "%A%i" t s
let fromString (s : string)
    =
    match (s.Substring (0,1), s.Substring (1, s.Length - 1)) with
    | ("R", n) -> (R, int n)
    | (_, n) -> (L, int n)

type Robot = Vector * Direction

let initialRobot (m : Map<Vector, Char>)
    =
    let robot (_,c) =
        List.contains c ['<';'^';'>';'v']
    let dir c =
        match c with
        | '<' -> W
        | '^' -> N
        | '>' -> E
        | _   -> S

    let (pos, char) =
        Map.toSeq m
        |> Seq.find robot

    (pos, dir char)

let possibleNext (r: Robot)
    =
    match r with // F, R, L
    | ((x, y), N) -> ((x    , y - 1), (x + 1, y    ), (x - 1, y    ))
    | ((x, y), E) -> ((x + 1, y    ), (x    , y + 1), (x    , y - 1))
    | ((x, y), S) -> ((x    , y + 1), (x - 1, y    ), (x + 1, y    ))
    | ((x, y), W) -> ((x - 1, y    ), (x    , y - 1), (x,     y + 1))

let turnLeft (r: Robot) : Robot 
    =
    match r with
    | (v, N) -> (v, W)
    | (v, E) -> (v, N)
    | (v, S) -> (v, E)
    | (v, W) -> (v, S)

let turnRight (r : Robot) : Robot
    =
    match r with
    | (v, N) -> (v, E)
    | (v, E) -> (v, S)
    | (v, S) -> (v, W)
    | (v, W) -> (v, N)

let rec pathToEnd 
    (map : Map<Vector,Char>) 
    (rbt : Robot) 
    (is : list<Instruction>)
    : list<Instruction>
    =
    let get v = Map.tryFind v map |> Option.defaultValue '.'
    let (f, r, v) = possibleNext rbt
    
    match (get f, get r, get v) with
    | ('.', '.', '.') -> is
    | ('#', _  , _)  -> pathToEnd map (f, snd rbt)    (fwd (List.head is) :: List.tail is)
    | (_  , '#', _)  -> pathToEnd map (turnRight rbt) ((R, 0) :: is)
    | _              -> pathToEnd map (turnLeft rbt)  ((L, 0) :: is)

let path = [|"R4"; "L12"; "L10"; "R12"; "R12"; "L4"; "L12"; "R12"; "L4"; "L12"; "R12";
    "R8"; "L10"; "R12"; "R8"; "L10"; "L4"; "L12"; "L10"; "R12"; "R12"; "L4";
    "L12"; "R12"; "L4"; "L12"; "R12"; "R8"; "L10"; "L4"; "L12"; "L10"; "R12"|]

let ps : seq<Instruction> = path |> Seq.map fromString

let asInput (is : seq<Instruction>)
    =
    let toInput (d, n) = string d + "," + string n
    let join (s : seq<string>) = String.Join (",", s |> Seq.toArray)
    
    Seq.map toInput is |> join

let prefixList (xs : list<'A>)
    =
    { 1 .. xs.Length }
    |> Seq.map (fun i -> List.take i xs)

let possiblePrefixes (is : list<Instruction>)
    =
    let possible x = (asInput x).Length < 21  
    prefixList is
    |> Seq.takeWhile possible

let row (m : Map<Vector, Char>) (y : int)
    =
    let onRow n ((_, y), _) = n = y
    let byX ((x, _), _) = x

    let r = 
        Map.toSeq m
        |> Seq.filter (onRow y)
        |> Seq.sortBy byX
        |> Seq.map snd
        |> Seq.toArray
    
    String.Join ("", r)

let showScaffold (m : Map<Vector, Char>)
    =
    let byY ((_, y), _) = y

    let (_, h) =
        Map.toSeq m
        |> Seq.maxBy byY
        |> fst

    { 0 .. h }
    |> Seq.map (row m)
    |> Seq.iter (printfn "%s")

[<EntryPoint>]
let main argv =
    let prg = (File.ReadAllText argv.[0]).Split([|','|])
                |> Array.map int64

    let ms = Array.create (1000 * 1000) 0L

    Array.set prg 0 2L
    let routine = "A,B,B,C,C,A,B,B,C,A"
    let fnA = "R,4,R,12,R,10,L,12"
    let fnB = "L,12,R,4,R,12"
    let fnC = "L,12,L,8,R,10"
    let feed = "n\n"
    let is = 
        String.Join("\n", [|routine; fnA;fnB;fnC;feed|])
        |> Seq.map (int64 << char)
        |> Seq.toList
    
    let (_, os', ms', _) = compute (init ms prg is) 0 
    
    // let m = model (os' |> List.rev |> List.map char)
    // let robot = initialRobot m
    // let path = 
    //     pathToEnd m robot []
    //     |> List.map showPath
        
    
    // showScaffold m
    // // printf "\n\n"
    // printfn "Initial robot: %A" robot
    // printfn "Path:"
    // List.rev path
    //     |> List.iter
    //        (fun x -> printfn "%s" x)

    printfn "space dust collected: %i" (Seq.head os')
        

    0
