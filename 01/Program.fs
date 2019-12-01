open System.IO

let flip f a b = f b a

let fuelCost = (flip (-) 2) << (flip (/) 3)

[<EntryPoint>]
let main argv =
    let r = File.ReadAllLines(argv.[0]) 
            |> Seq.map int
            |> Seq.fold (fun acc v -> acc + (fuelCost v)) 0

    printfn "%i" r
    0 
