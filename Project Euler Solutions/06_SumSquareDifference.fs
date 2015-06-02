namespace ProjectEuler.SumSquareDifference

module Solution =
    open System

    ///Computes square of one number
    let square (x:int64) = x * x

    ///Computes sum of squares from 1 to n
    let sumSquares n = 
                    [1L..n] 
                    |> List.map square
                    |> List.reduce(fun i j -> i + j)

    ///Computes the square of sum
    let squareSum n = 
                    [1L..n]
                    |> List.reduce(fun i j -> i + j)
                    |> square

    ///Computes difference between square of sum and sum of squares      
    let sumSquareDiff n = squareSum n - sumSquares n

///////////// INPUT OUTPUT //////////////////// 
    ///Reads input in Console, casts to int64
    let consoleReadInt() =  
                            let str  = Console.ReadLine()
                            try
                                    Some(str|> int64)
                            with
                                    ex-> None
    //Print output to Console:
    let consolePrint l = 
                         l |> Array.map(fun elem -> match elem with
                                                    | Some(n) -> sprintf "%d" n
                                                    | None -> "")
                           |> Array.map(printfn "%s")   

    ///////////// BUILD SOLUTION ////////////////////
    //Function for the solution:
    let solution (n:int64 option) = 
        match n with 
        | Some(n) 
            -> 
                //Cast to int to use List.init:
                let n' = int n
                Array.init n' (fun i -> consoleReadInt())   
                |> Array.map(fun elem -> Option.map sumSquareDiff elem)
                |> consolePrint
                |> ignore
        | None -> printfn "Bad Input"

//Build the program which runs in Console & HackerRank:
module Program = 
    open Solution
    
    //Main program:
    //[<EntryPoint>]
    let main argv = 
        let nbTest =  consoleReadInt()
        solution nbTest
        0
                    