// Learn more about F# at http://fsharp.org

open System
open System.IO
// timesteps: Infinite list
// 1 parse input -> moon gravity * velocity (3dCoordinate)
// 2 for every timestep, apply
//   - gravity
//     - pair all moons
//     - gravity gravity * gravity -> moon * moon
//     - apply velocity
// 3 scan over timesteps

let oneInput = "<x=-1, y=0, z=2>"

type Vector = int * int

type State
    = int
    * list<Vector>
    * list<Vector>
    * list<Vector>
    * Option<int>
    * Option<int>
    * Option<int>

let parsePoint (s : String) = 
    let trim c = List.contains c ['<';'=';'>'; ' '; 'x'; 'y'; 'z'] |> not
    let s' = 
        s |> String.filter trim
          |> (fun s -> s.Split (","))
    
    (int s'.[0],int s'.[1], int s'.[2])

let toVectors (ps : list<(int * int * int)>) =
    ps
    |> List.fold 
       (fun (xs, ys, zs) (x, y, z) -> ((x, 0) :: xs, (y, 0) :: ys, (z, 0) :: zs))
       ([],[],[])
    |> (fun (xs, ys, zs) -> 
       ( Seq.rev xs |> Seq.toList
       , Seq.rev ys |> Seq.toList
       , Seq.rev zs |> Seq.toList))

let initialState (xs, ys, zs) =
    (0, xs, ys, zs, None, None, None)  

let nudge a b =
    if a > b then -1
    elif a = b then 0
    else 1

let velocityChange (vs : list<Vector>) (v : Vector) =
    let v'' = List.allPairs [v] vs
              |> List.sumBy 
                 (fun ((a, _),(b, _)) -> nudge a b)

    (fst v, snd v + v'')

let positionChange (v, v') = (v + v', v')

let isNone x = 
    match x with
    | Some _ -> false
    | None   -> true 

let transformState (ix, iy, iz) (i, xs, ys, zs, rX, rY, rZ) =
    let i' = i + 1
    
    let xs' = xs |> List.map (positionChange << velocityChange xs)
    let ys' = ys |> List.map (positionChange << velocityChange ys)
    let zs' = zs |> List.map (positionChange << velocityChange zs)
    
    let rX' = if isNone rX && xs' = ix then Some i' else rX
    let rY' = if isNone rY && ys' = iy then Some i' else rY
    let rZ' = if isNone rZ && zs' = iz then Some i' else rZ
    
    (i', xs', ys', zs', rX', rY', rZ')

let rec gcd a b =
    if a = b then a
    elif a > b then gcd (a - b) b
    else gcd a (b - a) 

let lcm (a : int64) (b : int64) : int64 = (a * b) / gcd a b

[<EntryPoint>]
let main argv =
    let data = 
        File.ReadAllLines(argv.[0])
        |> Array.toList
        |> List.map parsePoint
        |> toVectors

    let comp =
        Seq.initInfinite ignore
        |> Seq.scan (fun s _ -> transformState data s) (initialState data)

    let (_, _, _, _, rX, rY, rZ) = 
        comp
        |> Seq.skipWhile
           (fun (_, _, _, _, rX, rY, rZ) -> isNone rX || isNone rY || isNone rZ)
        |> Seq.head

    match (rX, rY, rZ) with
        | (Some x, Some y, Some z) -> printfn "x: %i y: %i z: %i" x y z
                                      printfn "result: %i" (lcm (int64 z) (lcm (int64 x) (int64 y)))
        | _                        -> printfn "Program did not run to completion."

    0 // return an integer exit code