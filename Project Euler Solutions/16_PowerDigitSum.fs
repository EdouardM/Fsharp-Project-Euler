namespace ProjectEuler.PowerDigitSum

module Solution =
    open System
    open Checked 
    
    let toBigint (n:int) = bigint n
    let mult i j = i * j

    ///Computes power pow of n
    let power n pow = 
                      Array.create pow n 
                      |> Array.map toBigint
                      |> Array.reduce mult

    ///Turns number input in string then sum digits
    let digitsSum n = string n 
                      |> Array.ofSeq
                      |> Array.map (string >> int)
                      |> Array.reduce (+)

    ///////////// INPUT OUTPUT //////////////////// 
    ///Reads input in Console, casts to int64
    let consoleReadInt() =  Console.ReadLine() |> int

    let solution() = 
         let n = consoleReadInt() 
         Array.init n (fun i -> consoleReadInt())
         |> Array.map (fun i -> power 2 i |> digitsSum)
         |> Array.map (fun i -> printfn "%d" i)
         |> ignore
    
//Build the program which runs in Console & HackerRank:
module Program = 
    open Solution
    
    //Main program:
    //[<EntryPoint>]
    let main argv = 
        solution()
        0


