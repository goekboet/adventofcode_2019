// Learn more about F# at http://fsharp.org

open System
open System.IO
open Microsoft.FSharp.Core.Operators.Checked

let reduceLayer l = 
    let ones = (List.filter ((=) 1) l).Length
    let twos = (List.filter ((=) 2) l).Length
    
    ones * twos

let charToInt (c : char) = int c - int '0'

let getLayer (s : list<char>) = 
    s
    |> List.map charToInt
    |> List.chunkBySize (6 * 25)
    |> List.minBy (fun x -> (List.filter ((=) 0) x).Length)
    |> reduceLayer

[<EntryPoint>]
let main argv =
    let inp = File.ReadAllText argv.[0]
    let r = getLayer (List.ofSeq inp)

    printfn "Result: %i" r
    0 // return an integer exit code
