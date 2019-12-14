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

type Point = int * int * int

let parsePoint (s : String) = 
    let trim c = List.contains c ['<';'=';'>'; ' '; 'x'; 'y'; 'z'] |> not
    let s' = 
        s |> String.filter trim
          |> (fun s -> s.Split (","))
    
    (int s'.[0],int s'.[1], int s'.[2])

let sumPoint ( x, y, z ) = abs x + abs y + abs z
let showPoint ( x, y, z ) = sprintf "(%4i, %4i, %4i)" x y z

type Gravity = Point
type Velocity = Point

type Moon = Gravity * Velocity
let showMoon ((g, v) : Moon) = sprintf "g: %s - v: %s" (showPoint g) (showPoint v)

let nudge a b =
    if a > b then -1
    elif a = b then 0
    else 1

let applyGravity (((x,y,z), _) : Moon) (((x',y',z'), _) : Moon) = 
    (nudge x x',nudge y y',nudge z z')

let runGravity (ms : list<Moon>) (i : int) = 
    let pair (xs : list<Moon>) (x : Moon) =
        List.allPairs [ x ] xs
        |> List.fold (fun (g, (vx, vy, vz)) (m, m') ->
           let (vx', vy', vz') = applyGravity m m'
           in (g, (vx + vx', vy + vy', vz + vz')))
           x

    ms |> List.map (pair ms) 

let applyVelocity (((gx, gy, gz),(vx, vy, vz)) : Moon) = 
    ((gx + vx, gy + vy, gz + vz ), (vx, vy, vz)) 

let runVelocity = List.map applyVelocity 

let init (data : array<Point>) =
    data
    |> Seq.map (fun (x : Point) -> (x, ( 0, 0, 0 )))
    |> Seq.toList

let run (ms : list<Moon>) = 
    Seq.initInfinite id
        |> Seq.scan (fun x i -> runVelocity <| runGravity x i) ms

let showstate (s : list<Moon>) = 
    s |> Seq.mapi (fun i x -> sprintf "%i: %s" i (showMoon x))
      |> (fun ss -> String.Join("\n", Seq.toArray ss))

let getPotentialEnergy (m : Moon) = 0
let getKineticEnergy (m : Moon) = 0

[<EntryPoint>]
let main argv =
    let data = 
        File.ReadAllLines(argv.[0])
        |> Array.map parsePoint

    let state = init data 
    let r = 
        run state
        |> Seq.take 1001
        
    r   |> Seq.iteri (fun i x -> 
            if (i % 100) = 0 
            then printfn "step: %i\n%s\n" i (showstate x))
            
    // let longs = Seq.unfold (fun s -> Some (s, s + 1L)) 0L
    // longs |> Seq.skipWhile (fun l -> l < 9223372036854775807L) |> Seq.take 3
    
    let pT = 
        Seq.last r
        |> Seq.sumBy (fun (g, v) -> (sumPoint g) * (sumPoint v))
           
    printfn "Total: %i" pT
    0 // return an integer exit code
