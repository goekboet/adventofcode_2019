// Learn more about F# at http://fsharp.org

open System
open System.IO

let char2int c = int c - int '0'

let pattern2 (x : int) (y: int)
    =
    let p = [0;1;0;-1]
    p.[((x + 1) / (y + 1) ) % p.Length]

let pattern w (h : int) : seq<int> =
    let p = [0;1;0;-1]
    seq { for i in 0 .. (w - 1) do yield p.[(((i+1) / (h + 1)) % 4)] }

let applypattern (is : list<int>) (y : int)  
    =
    let mul (a,b) = a * b
    let p = pattern is.Length y
    Seq.zip is p
    |> Seq.map mul

let phase (is : list<int>)
    =
    let lastd i = (%) (abs i) 10

    seq { 0 .. is.Length - 1 }
    |> Seq.map (lastd << Seq.sum << applypattern is)
    |> Seq.toList

let phaseFromRight (is : list<int>)
    =
    let next (v : int) (s : option<int>)
        =
        match s with
        | Some x -> let r = (v + x) % 10 in (r, Some r)
        | None   -> (v           , Some v) 
    
    List.mapFoldBack next is None
    |> fst

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

let toString (sep : string) (arr : array<string>) = String.Join (sep, arr)

let showphases (is : list<int>) count 
    =
    let fmt = toString "" << Seq.toArray << Seq.map (sprintf "%i")

    { 1 .. count }
    |> Seq.scan (fun s _ -> phaseFromRight s) is
    |> Seq.map fmt
    |> Seq.toArray
    |> toString "\n"

let repeatInput (is : list<int>) (i : int)
    =
    let is' = is |> List.toArray
    is'.[i % is'.Length]

let offset (data : list<int>) 
    =
    let digits = List.take 7 data |> List.toArray
    String.Join ("", digits) |> int
    

[<EntryPoint>]
let main argv =
    let data = 
        File.ReadAllText argv.[0]
        |> Seq.map char2int
        |> Seq.toList

    let off = offset data

    let signal = 
        seq { off .. (data.Length * 10000) - 1 }
        |> Seq.map (repeatInput data)
        |> Seq.toList

    printfn "offset:        %i" off
    printfn "signal tail:   %i" signal.Length
    printfn "total          %i" (off + signal.Length)

    let phases =
        seq { 1 .. 100 }
        |> Seq.scan (fun s _ -> phaseFromRight s) signal

    let show is = 
        String.Join("", is |> Seq.map string |> Seq.take 8 |> Seq.toArray) 

    let r = 
        Seq.rev phases
        |> Seq.head
        |> show

    printfn "result:        %s" r
    0 // return an integer exit code
