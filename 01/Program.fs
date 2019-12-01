open System.IO
open Microsoft.FSharp.Core.Operators.Checked

let flip f a b = f b a

let fuelCost = (flip (-) 2) << (flip (/) 3)

let rec compFuelCost (x) =
       match (x < 9) with
        | true -> 0
        | false -> let c = fuelCost x in c + compFuelCost c
        
         

[<EntryPoint>]
let main argv =
    let r = File.ReadAllLines(argv.[0]) 
            |> Seq.map int
            |> Seq.fold (fun acc v -> acc + (compFuelCost v)) 0

    printfn "%i" r
    0 
