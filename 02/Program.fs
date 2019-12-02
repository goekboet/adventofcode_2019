open System
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

    

let cartesian xs ys = xs |> List.collect (fun x -> ys |> List.map (fun y -> x, y))

let runComp xs (a, b) =
    let arr = Array.copy xs
    Array.set arr 1 a
    Array.set arr 2 b

    (compute 0 arr).[0]
    



[<EntryPoint>]
let main argv =
    let xs = (File.ReadAllText argv.[0]).Split([|','|])
                   |> Array.map int

    printfn "12 2 -> %i" (runComp xs (12,2))
    let pairs = cartesian [0..99] [0..99]
    let ((a, b), _) = List.map (fun x -> (x, runComp xs x)) pairs
                       |> List.find (fun (_, r) -> r = 19690720)

    printfn "(%i, %i)" a b
    printfn "%i" (100 * a + b)
                
    
    // printf "%s" <| String.Join ("\n", (List.map string inputs))
    

    // Array.set initial 1 12
    // Array.set initial 2  2

    // let r = compute 0 initial
    // printfn "%s" <| String.Join (", ", Array.map string r)    

    // printfn "%i" r.[0]

    0 // return an integer exit code