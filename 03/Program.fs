// Learn more about F# at http://fsharp.org

open System
open System.IO


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

let follow cs d = 
    let o = Seq.last cs
    let p = path o d
    Seq.append cs p 

let run dirs = Seq.fold follow (Seq.singleton (0,0))  dirs

[<EntryPoint>]
let main argv =
    let dirs = (File.ReadAllText <| argv.[0]).Split ","
                  |> Seq.map parseDirection

    let r = run dirs
    let display = Seq.map string r |> Seq.toArray
    printfn "%s" (String.Join ("\n", display))  
    
    0 // return an integer exit code
