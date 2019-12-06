// Learn more about F# at http://fsharp.org

open System
open System.IO
open Microsoft.FSharp.Core.Operators.Checked

type Name = string

type SpaceObject =
    | Leaf of Name 
    | Orbit of Name * seq<SpaceObject>

type Spacemap = seq<Name * Name>

let parse (s : string) = (s.[..2], s.[4..])

let findCom m =
    let (ps, cs) = (Seq.map fst m, Seq.map snd m)

    Seq.filter (fun x -> Seq.contains x cs |> not) ps
      |> Seq.exactlyOne

let getChildren (m : Spacemap) (n : Name) = Seq.filter ((=) n << fst) m |> Seq.map snd

let rec makeSpaceObj m n =
    let cs = getChildren m n
    if Seq.isEmpty cs
        then Leaf n
        else Orbit (n, (Seq.map (fun x -> makeSpaceObj m x) cs))

let rec count n (o : SpaceObject) =
    match o with
        | Leaf _        -> n
        | Orbit (_, cs) -> n + (Seq.sumBy (fun x -> count (n + 1) x) cs)    

[<EntryPoint>]
let main argv =
    let orbits = (File.ReadAllLines argv.[0])
                |> Array.map parse
    let r = makeSpaceObj orbits (findCom orbits) |> count 0 
    
    printfn "Checksum %i" r
    0 // return an integer exit code
