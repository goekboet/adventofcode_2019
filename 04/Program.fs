// Learn more about F# at http://fsharp.org

open System

let first = 124075
let last = 580769 

let streaks acc v =
    match acc with
        | []          -> [[v]]
        | h :: rest   -> let last = Seq.head h in if last = v then (v :: h) :: rest else [v] :: (h :: rest)

let anyPairs xs =
    Seq.fold streaks [] xs
        |> Seq.map Seq.length
        |> Seq.exists ((=) 2)
        
let check xs = 
    let pw = Seq.pairwise xs 
    in not (Seq.exists (fun (a, b) -> a > b) pw) && anyPairs xs

[<EntryPoint>]
let main argv =
    let r = seq { first .. last }
              |> Seq.map (check << string)
              |> Seq.filter id
              |> Seq.length
    
    printfn "%i" r
    0 // return an integer exit code
