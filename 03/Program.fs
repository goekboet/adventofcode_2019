// Learn more about F# at http://fsharp.org

open System

type Direction = Up | Down | Left | Right

type Instruction = Direction * int

let parseDirection (s:string) = 
    match (s.Substring(0, 1), int <| s.Substring(1, s.Length - 1)) with
        | ("U", d) -> (Up, d)
        | ("D", d) -> (Down, d)
        | ("L", d) -> (Left, d)
        | ("R", d) -> (Right, d)
        | _ -> (Up, 0)
        
let path (x, y) (dir, dis) =
    match dir with
        | Up    -> Seq.init dis (fun n -> y + n + 1) |> Seq.map (fun v -> (x, v))
        | Down  -> Seq.init dis (fun n -> y - n - 1) |> Seq.map (fun v -> (x, v))
        | Left  -> Seq.init dis (fun n -> x - n - 1) |> Seq.map (fun v -> (v, y))
        | Right -> Seq.init dis (fun n -> x + n + 1) |> Seq.map (fun v -> (v, y))

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
