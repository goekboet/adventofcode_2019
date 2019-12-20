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
    
let showIs (is : seq<int>) = String.Join ("", Seq.map string is |> Seq.toArray)

let first8 (is : seq<int>) = Seq.take 8 is |> showIs

let pattern w (h : int) : seq<int> =
    let p = [0;1;0;-1]
    seq { for i in 0 .. (w - 1) do yield p.[(((i+1) / (h + 1)) % 4)] }

let showpattern w h
    =
    let p x y = 
        pattern x y 
        |> Seq.map (sprintf "%3i") 
        |> Seq.toArray
        |> (fun ss -> String.Join ("", ss))
    
    let r = 
        seq { 0 .. (h - 1) }
        |> Seq.map (p w)
        |> Seq.toArray

    String.Join("\n", r)
    
let applypattern (is : list<int>) (y : int)  
    =
    let mul (a,b) = a * b
    let p = pattern is.Length y
    Seq.zip is p
    |> Seq.map mul

let showAppliedPattern (is : list<int>) h
    =
    let p is y =
        applypattern is y
        |> Seq.map (sprintf "%3i") 
        |> Seq.toArray
        |> (fun ss -> String.Join ("", ss))

    let r =
        seq { 0 .. (h - 1) }
        |> Seq.map (p is)
        |> Seq.toArray

    String.Join("\n", r)
    
let phase2 (is : list<int>)
    =
    let lastd i = (%) (abs i) 10

    seq { 0 .. is.Length - 1 }
    |> Seq.map (lastd << Seq.sum << applypattern is)

[<EntryPoint>]
let main argv =
    let data = 
        File.ReadAllText argv.[0]
        |> Seq.map char2int

    let data' = Seq.collect (fun _ -> data) {0 .. 999} |> Seq.toList
    let offset = Seq.take 7 data' |> showIs |> int
    printfn "offset: %i" offset

    let drive = Seq.initInfinite id
    let is = Seq.scan (fun s _ -> compute s) data' drive |> Seq.skip 100 |> Seq.head
    
    let r = Seq.skip offset is |> first8
    printfn "%s" r
    // Seq.map showIs is
    //   |> Seq.iteri (fun i x -> printfn "%5i %s" i x)

    //printfn "%s" (first8 is)
    0 // return an integer exit code
