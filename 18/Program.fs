﻿// Learn more about F# at http://fsharp.org

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

type KeySet = int32

let emptyKeySet : KeySet = 0

let addKey 
    (set : KeySet) 
    (k : Key)
    =
    let o = 1 <<< ((int k) - (int 'a'))
    set ||| o

let fromKeys
    (keys : seq<Key>)
    =
    Seq.fold addKey emptyKeySet keys

let isSubset
    (s : KeySet)
    (s' : KeySet)
    =
    (s &&& s') = s'

let containsKey
    (k : Key)
    (set : KeySet)
    =
    let s' = 1 <<< ((int k) - (int 'a'))
    isSubset set s' 

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
    (doors : KeySet)
    : KeySet
    =
    let t = Map.find cv map
    if cd = 0
    then doors
    else
        let vis' = Map.remove cv vis
        let doors' = if isDoor t then addKey doors (Char.ToLower t) else doors
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
    
type Edge = Key * Distance * KeySet

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
            let ks = traceBack map (v, d) vis emptyKeySet
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
    (candidates : list<Key * Distance * KeySet>)
    (keyring : KeySet)
    =
    candidates
    |> List.filter (fun (k, _, ds) -> isSubset keyring ds && (containsKey k keyring |> not))
    |> List.map (fun (v, d, _) -> (v, d))
    |> List.sortBy (fun (_, d) -> d)

let rec traverse
    (lookup : Map<Key, list<Edge>>)
    (keyring : KeySet)
    (current : Key)
    (distance : Distance)
    =
    let keyring' = if current <> '@' then addKey keyring current else keyring
    let edges = Map.find current lookup
    let next = chooseEdges edges keyring'
    
    if List.isEmpty next
    then Leaf distance
    else
        Node (distance, Seq.map (fun (k, d) -> traverse lookup keyring' k (distance + d)) next)

let rec toList
    (acc : list<int>)
    (t : Tree)
    =
    match t with
    | Leaf d -> d :: acc
    | Node (_, ns) -> Seq.fold toList acc ns 

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


let rec djikstra
    (lookup : Map<Key, list<Edge>>)
    (allKeys : KeySet)
    (distances : Map<Key * KeySet, Distance>)
    (current : Key * KeySet * Distance)
    =
    let addIfCloser 
        (ds : Map<Key * KeySet, Distance>)
        ((k, ks, d) : Key * KeySet * Distance)
        : Map<Key * KeySet, Distance>
        =
        let d' = 
            Map.tryFind (k, ks) ds
            |> Option.map (fun d' -> if d < d' then d else d')
            |> Option.defaultValue d

        Map.add (k, ks) d' ds 

    let (cK, cKs, cD) = current
    let distances' = Map.remove (cK, cKs) distances
    let cKs' = if cK <> '@' then addKey cKs cK else cKs
    if (cKs' = allKeys) 
        then 
            cD
        else 
        let edges = Map.find cK lookup
        let distances'' = 
            chooseEdges edges cKs'
            |> List.map (fun (k, d) -> (k, addKey cKs k, cD + d))
            |> List.fold addIfCloser distances'

        let ((nK, nKs), nD) =
            distances''
            |> Map.toSeq
            |> Seq.minBy (fun (_,d) -> d)

        djikstra lookup allKeys distances'' (nK, nKs, nD)

[<EntryPoint>]
let main argv =
    let map = readMap argv.[0]

    let keys = getKeys map
    let lookup = pathMap map keys
    let keyring = emptyKeySet
    let allKeys =
        keys
        |> List.map fst
        |> List.filter ((<>) '@')
        |> fromKeys

    let distance = 0
    let ds = 
        [(('@', 0), 0)]
        |> Map.ofList

    let r' = djikstra lookup allKeys ds ('@', 0, 0)
    
    printfn "%i" r'

    // let t = traverse lookup keyring '@' distance

    // let r = 
    //     t
    //     |> getMin None
    //     |> Option.map string
    //     |> Option.defaultValue "n/a"

    // printfn "%s" r
    
    0 // return an integer exit code
