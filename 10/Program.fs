// Learn more about F# at http://fsharp.org

open System
open System.IO

type Map = string[]
type Position = int * int
type AsteroidPositions = Set<Position>

let pair x y = (x, y)
let place ((x : int), (ys : seq<char>)) = 
    Seq.mapi (fun i y -> ((x, i), y = '#' )) ys
    |> Seq.filter snd
    |> Seq.map fst

let readPos (map : Map) =
    Seq.mapi pair map
    |> Seq.collect place
    |> Set.ofSeq

[<EntryPoint>]
let main argv =
    let map = File.ReadAllLines("test.txt")
    let r = readPos map
    printfn "Asteroids: %A" r
    0 // return an integer exit code
