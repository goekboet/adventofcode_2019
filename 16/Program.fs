// Learn more about F# at http://fsharp.org

open System
open System.IO

let basePattern = [0;1;0;-1]

let char2int c = int c - int '0'

let LastDigit = char2int << Seq.last << string

let nthPattern n =
    Seq.initInfinite (fun _ -> Seq.collect (fun x -> Seq.replicate n x) basePattern)
    |> Seq.collect id
    |> Seq.skip 1
 
let phase (n : int) (is : seq<int>) 
    =
    Seq.zip is (nthPattern n)
    |> Seq.sumBy (fun (a,b) -> a * b)
    |> LastDigit

let compute (is : seq<int>)
    =
    {1 .. Seq.length is}
    |> Seq.map (fun i -> phase i is)
    |> Seq.toList
    
let showIs (is : list<int>) = String.Join ("", Seq.map string is |> Seq.toArray)

let first8 (is : list<int>) = List.take 8 is |> showIs

[<EntryPoint>]
let main argv =
    let data = 
        File.ReadAllText argv.[0]
        |> Seq.map char2int
        |> Seq.toList

    let drive = Seq.initInfinite id
    let is = Seq.scan (fun s _ -> compute s) data drive |> Seq.skip 100 |> Seq.head
    
    // Seq.map showIs is
    //   |> Seq.iteri (fun i x -> printfn "%5i %s" i x)

    printfn "%s" (first8 is)
    0 // return an integer exit code
