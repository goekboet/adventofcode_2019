// Learn more about F# at http://fsharp.org

open System
open System.IO
open Microsoft.FSharp.Core.Operators.Checked

type ParamMode = Ref | Imm
let pMode x = if x = '0' then Ref else Imm

type Pos = int
type Op = ParamMode * int

type Opcode 
    = Add of Op * Op * Pos
    | Mul of Op * Op * Pos
    | Inp of Pos
    | Out of Pos
    | Hal

let width c =
    match c with
        | Add _ -> 4
        | Mul _ -> 4
        | Inp _ -> 2
        | Out _ -> 2
        | Hal   -> 0


let parseOpcode p (xs : int[]) = 
    let c = sprintf "%05i" (xs.[p])
    match (int c.[3..], c.[..2]) with
        | (1, modes) -> Add ((pMode modes.[2], xs.[p + 1]), (pMode modes.[1], xs.[p + 2]), xs.[p + 3])
        | (2, modes) -> Mul ((pMode modes.[2], xs.[p + 1]), (pMode modes.[1], xs.[p + 2]), xs.[p + 3])
        | (3, _    ) -> Inp xs.[p + 1]
        | (4, _    ) -> Out xs.[p + 1]
        | _          -> Hal





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
    printfn "Hello World from F#!"
    0 // return an integer exit code
