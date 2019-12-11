// Learn more about F# at http://fsharp.org

open System
open System.IO

type Map = string[]
type Position = int * int
type AsteroidPositions = List<Position>
type Line = seq<Position>
type AsteroidPair = Position * Position

let place ((y : int), (xs : seq<char>)) = 
    Seq.mapi (fun x c -> ((x, y), c )) xs
        |> Seq.filter (fun x -> snd x = '#') 
        |> Seq.map fst
    

let readPos (map : Map) =
    Seq.mapi (fun y line -> (-y, line) ) map
    |> Seq.collect place
    |> Set.ofSeq

let rec gcd a b =
    if a = b then a
    elif a > b then gcd (a - b) b
    else gcd a (b - a)

let inLine (x', y') (x'', y'') =
    let dx = x'' - x'
    let dy = y'' - y'

    let sign x = if x < 0 then -1 else 1

    if dx = 0 
        then Seq.init (abs dy + 1) (fun i -> (x', y' + (sign dy * i)) )
    elif dy = 0 
        then Seq.init (abs dx + 1) (fun i -> (x' + (sign dx * i), y') )
    else
        let gcd = gcd (abs dx) (abs dy)
        let (nx, ny) = (dx / gcd, dy / gcd) 
        Seq.init (gcd + 1) (fun i -> (x' + (nx * i), y' + (ny * i))) 

let firstCollision (map : Set<Position>) (o : Position, e : Position) =
    let coll c = Set.contains c map
    
    inLine o e
       |> Seq.skip 1
       |> Seq.find coll

let allPairs (xs : AsteroidPositions) =
    List.collect (fun p -> List.choose (fun p' -> if p' = p then None else Some(p, p')) xs) xs
     |> List.groupBy fst

let mostLineOfSights (map : Set<Position>) =
    allPairs (Set.toList map) 
        |> List.map (fun (k, ps) -> (k, (List.map (fun p -> firstCollision map p) ps |> List.distinct).Length ))
        |> List.maxBy snd    

[<EntryPoint>]
let main argv =
    let map = File.ReadAllLines(argv.[0])
                |> readPos
               
    let r = mostLineOfSights map
    
    printfn "Monitor: %s" <| string r
    0 // return an integer exit code
