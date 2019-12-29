// Learn more about F# at http://fsharp.org

open System
open System.IO

type Vector = (int * int)
type Tile = char

let toMap (s : string) : Map<Vector, Tile>
    =
    let rs = s.Split("\n")
    let vectorize y s = 
        s 
        |> Seq.toList
        |> Seq.mapi (fun x c -> ((x, y), c))

    rs
    |> Seq.mapi vectorize
    |> Seq.collect id
    |> Map.ofSeq
  
let getAdjacent 
    (map : Map<Vector, Char>) 
    ((x, y): Vector) 
    : list<Vector>
    =
    let get v = 
        Map.tryFind v map
        |> Option.defaultValue '#'

    let path c = c <> '#' 

    [(x + 1, y);(x - 1, y);(x, y + 1);(x, y - 1)]
    |> List.filter (path << get)
    
    
type Key = Char
let isKey = Char.IsLower
type Door = Char
let isDoor = Char.IsUpper

type Distance = int

let addIfCloser 
    (map : Map<Vector, Distance>) 
    ((k : Vector), (d : Distance))
    =
    let known = Map.tryFind k map
    match known with
    | None -> Map.add k d map
    | Some d' -> if d < d' then Map.add k d map else map

let rec traceBack
    (map : Map<Vector, Tile>)
    ((cv, cd) : Vector * Distance)
    (vis : Map<Vector,Distance>)
    (doors : Set<Key>)
    : Set<Key>
    =
    let t = Map.find cv map
    if cd = 0
    then doors
    else
        let vis' = Map.remove cv vis
        let doors' = if isDoor t then Set.add (Char.ToLower t) doors else doors
        let adj = getAdjacent map cv
        let path cand (v, _) = List.contains v cand 
        let curr' = 
            vis'
            |> Map.filter (fun k _ -> List.contains k adj)
            |> Map.toSeq
            |> Seq.minBy snd

        traceBack map curr' vis' doors'

let pair 
    (keys : list<Key * Vector>)
    : list<Key * list<Key * Vector * Vector>>
    =
    let node (k, v) (k', v') = 
        if k <> k' && k' <> '@' 
        then Some (k, v, v') 
        else None
    
    keys
    |> List.collect (fun k -> 
           List.choose (node k) keys
           |> List.groupBy (fun (k,_,_) -> k))
    
type Edge = Key * Distance * Set<Key>

let rec pathToKey
    (map : Map<Vector, Tile>)
    (adj: list<Vector * Distance>)
    (target : Vector)
    (vis: Map<Vector,Distance>)
    : option<Edge>
    =
    match adj with
    | [] -> None
    | (v, d) :: vs ->
        if (v = target)
        then
            let ks = traceBack map (v, d) vis Set.empty
            let k = Map.find v map 
            Some (k, d, ks)
        else
            let vis' = addIfCloser vis (v, d)
            let visited m v = Map.containsKey v m
            let adj' = 
                getAdjacent map v
                |> List.filter (not << visited vis')
                |> List.map (fun v -> (v, d + 1))
                |> List.append vs

            pathToKey map adj' target vis'

let pathMap
    (map : Map<Vector, Tile>)
    (keys : list<Key * Vector>)
    : Map<Key, list<Edge>>
    =
    pair keys
    |> List.map (fun (k, ns) -> (k, List.choose (fun (_, v, v') -> pathToKey map [(v, 0)] v' Map.empty) ns))
    |> Map.ofList

type Tree =
    | Leaf of Distance
    | Node of Distance * Tree seq     

let chooseEdges
    (candidates : list<Key * Distance * Set<Key>>)
    (keyring : Set<Key>)
    =
    candidates
    |> List.filter (fun (k, _, ds) -> Set.isSubset ds keyring && Set.contains k keyring |> not)
    |> List.map (fun (v, d, _) -> (v, d))

let rec traverse
    (lookup : Map<Key, list<Edge>>)
    (keyring : Set<Key>)
    (current : Key)
    (distance : Distance)
    =
    let keyring' = Set.add current keyring
    let edges = Map.find current lookup
    let next = chooseEdges edges keyring'
    
    if List.isEmpty next
    then Leaf distance
    else
        Node (distance, Seq.map (fun (k, d) -> traverse lookup keyring' k (distance + d)) next)

let rec getMin 
    (running : Option<Distance>)
    (t : Tree) 
    =
    let tryRunning r v = 
        if r |> Option.map ((<) v) |> Option.defaultValue true
        then Some v
        else r

    let discardBranch r b =
        r |> Option.map ((>) b) |> Option.defaultValue false

    match t with
    | Leaf d -> tryRunning running d
    | Node (d, cs) -> 
        if discardBranch running d
        then running 
        else Seq.fold getMin running cs
    
let getKeys 
    (map : Map<Vector, Tile>)
    : list<Key * Vector>
    =
    let isPoi c = Char.IsLower c || c = '@' 
    map
    |> Map.toSeq
    |> Seq.filter (isPoi << snd)
    |> Seq.map (fun (v, c) -> (c, v))
    |> Seq.toList


let readMap p 
    = 
    File.ReadAllText p
    |> toMap

[<EntryPoint>]
let main argv =
    let map = readMap argv.[0]

    let keys = getKeys map
    let lookup = pathMap map keys
    let keyring = Set.empty
    let distance = 0
    
    let r = 
        traverse lookup keyring '@' distance
        |> getMin None
        |> Option.map string
        |> Option.defaultValue "n/a"

    printfn "%s" r
    
    0 // return an integer exit code
