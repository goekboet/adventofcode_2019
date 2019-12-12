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

type Polar = float * float

let toPolar (x, y) =
    let r = sqrt (float (x * x + y * y))
    let t = atan2 (float y) (float x)

    if t < 0.0
    then (r, t + (2.0 * Math.PI))
    else (r, t)

let ccw90 (r, t) = 
    let (r, t') = (r, t - Math.PI * (1.0 / 2.0))
    if t' <= 0.0
    then (r, t' + Math.PI * 2.0)
    else (r, t')

let parsemap (map : Map) =
    let w = map.[0].Length
    
    let o = Seq.collect id map
                 |> Seq.indexed
                 |> Seq.map (fun (i, x) -> ((i % w, i / w), x))
                 |> Seq.find ((=) 'X' << snd)
                 |> fst

    let mapping (x, y) w (i, _) =
        let (x', y') = (i % w, i / w)
        let polar = toPolar (x' - x, y - y') |> ccw90
        
        (polar, (x', y'))

    Seq.collect id map
    |> Seq.indexed
    |> Seq.filter ((=) '#' << snd)
    |> Seq.map (mapping o w)

[<EntryPoint>]
let main argv =
    let data = File.ReadAllLines(argv.[0])

    // let best = 
    //     data
    //     |> readPos
    //     |> mostLineOfSights
    
    let xs = 
        data
        |> parsemap
    
    let mapping xs = 
        Seq.sortBy (fun ((r, t), _) -> r) xs
        |> Seq.map (fun (_, x) -> x)
        |> Seq.toList

    let xs' = 
        xs 
        |> Seq.groupBy (fun ((_, t), _) -> t)
        |> Seq.sortByDescending fst
        |> Seq.map (fun (_, xs) -> mapping xs)
        |> Seq.toArray

    let order ((xs : list<Position>[]), rs) (i : int) =
        match xs.[i] with
            | x :: r  -> 
                Array.set xs i r
                (xs , x :: rs)
            | []      ->
                (xs, rs)

    let (x, y) =
        Seq.initInfinite (fun i -> i % xs'.Length)
        |> Seq.scan order (xs', [])
        |> Seq.skip (Seq.length xs - 1)
        |> Seq.map snd
        |> Seq.head
        |> Seq.rev
        |> Seq.skip 199
        |> Seq.head

    printfn "%i" (x * 100 + y)

    // Seq.iteri (fun i x -> printfn "%4i %A" i x) r
              
    0 // return an integer exit code
