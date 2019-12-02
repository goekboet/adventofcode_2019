open System.IO
open Microsoft.FSharp.Core.Operators.Checked

let opCode1 (p:int) (xs:int[]) =
    let (lhs, rhs, reg) = (xs.[p + 1], xs.[p + 2], xs.[p + 3])
    // printfn "p: %i lhs: %i rhs %i reg %i" p lhs rhs reg

    Array.set xs reg (xs.[lhs] + xs.[rhs])

    xs

 

let opCode2 (p:int) (xs:int[]) =
    let (lhs, rhs, reg) = (xs.[p + 1], xs.[p + 2], xs.[p + 3])

    Array.set xs reg (xs.[lhs] * xs.[rhs])

    xs

 

let rec compute (p:int) (xs:int[]) =

    match xs.[p] with
        | 1  -> let xs2 = opCode1 p xs in compute (p + 4) xs2
        | 2  -> let xs2 = opCode2 p xs in compute (p + 4) xs2
        | 99 -> xs
        | _  -> [||]

    

 

[<EntryPoint>]

let main argv =

    let initial = (File.ReadAllText argv.[0]).Split([|','|])
                   |> Array.map int

    Array.set initial 1 12
    Array.set initial 2  2

    let r = compute 0 initial
    // printfn "%s" <| String.Join (", ", Array.map string r)    

    printfn "%i" r.[0]

    0 // return an integer exit code