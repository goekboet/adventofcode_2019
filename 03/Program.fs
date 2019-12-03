﻿// Learn more about F# at http://fsharp.org

open System
open System.IO


type Direction = Up | Down | Left | Right

type Instruction = Direction * int

let parseDirection (s:string) = 
    match (s.Substring(0, 1), int <| s.Substring(1, s.Length - 1)) with
        | ("U", d) -> (Up, d)
        | ("D", d) -> (Down, d)
        | ("L", d) -> (Left, d)
        | ("R", d) -> (Right, d)
        | _ -> (Up, 0)
        
let path (x, y) (dir, dis) =
    match dir with
        | Up    -> Seq.init dis (fun n -> y + n + 1) |> Seq.map (fun v -> (x, v))
        | Down  -> Seq.init dis (fun n -> y - n - 1) |> Seq.map (fun v -> (x, v))
        | Left  -> Seq.init dis (fun n -> x - n - 1) |> Seq.map (fun v -> (v, y))
        | Right -> Seq.init dis (fun n -> x + n + 1) |> Seq.map (fun v -> (v, y))

let follow cs d = 
    let o = Seq.last cs
    let p = path o d
    Seq.append cs p 

let run dirs = Seq.fold follow (Seq.singleton (0,0))  dirs

let manhattanD (a, b) = abs a + abs b

type Direction = Up | Down | Left | Right

type Instruction = Direction * int

let parseDirection (s:string) = 
    match (s.Substring(0, 1), int <| s.Substring(1, s.Length - 1)) with
        | ("U", d) -> (Up, d)
        | ("D", d) -> (Down, d)
        | ("L", d) -> (Left, d)
        | ("R", d) -> (Right, d)
        | _ -> (Up, 0)
        
let path (x, y) (dir, dis) =
    match dir with
        | Up    -> Seq.init dis (fun n -> y + n + 1) |> Seq.map (fun v -> (x, v))
        | Down  -> Seq.init dis (fun n -> y - n - 1) |> Seq.map (fun v -> (x, v))
        | Left  -> Seq.init dis (fun n -> x - n - 1) |> Seq.map (fun v -> (v, y))
        | Right -> Seq.init dis (fun n -> x + n + 1) |> Seq.map (fun v -> (v, y))

[<EntryPoint>]
let main argv =
    let dirs = (File.ReadAllText <| argv.[0]).Split ","
                  |> Seq.map parseDirection

    let players = (File.ReadLines argv.[0])
                    |> Seq.map (fun x -> x.Split ",")
                    |> Seq.map (Seq.map parseDirection)

    let p1 = Seq.head players |> run |> Set.ofSeq
    let p2 = (Seq.head << Seq.skip 1) players |> run |> Set.ofSeq
    let cross = Set.intersect p1 p2 
                  |> Set.map manhattanD
                  |> Set.filter (fun x -> x <> 0)
                  |> Set.maxElement 

    //let r = run dirs
    //let display = Set.map string cross |> Seq.toArray
    printfn "%i" cross  
    
    0 // return an integer exit code
