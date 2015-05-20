namespace ProjectEuler.LargestPrimeFactor

module Solution =
    open System
    open Checked 
    ///////////// CORE LOGIC ////////////////////

    ///Tells if a number is a Prime number
    let isPrime n = 
                let rec testprime i =   if n % i = 0L then false
                                        elif i >= n/2L then true
                                        else testprime (i+1L)
                testprime 2L
    
    ///Builds a sequence of prime factors and picks the max
    let largestPrimeFactor n = 
                        let res =   [1L.. n/2L]  |> Seq.filter (fun i -> n % i = 0L)
                                                 |> Seq.filter isPrime
                                                 |> Seq.max
                        if res = 1L then n else res

    ///////////// INPUT OUTPUT ////////////////////
    //Read input in Console, cast to int64:
    let consoleReadInt() =  
                            let str  = Console.ReadLine()
                            try
                                    Some(str|> int64)
                            with
                                    ex-> 
                                        printfn "Bad input: %s" str
                                        None

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
                | Some(n) -> 
                            //Cast to int to use List.init:
                            let n' = int n
                            Array.init n' (fun i -> consoleReadInt()) |> Array.map(fun elem -> Option.map largestPrimeFactor elem)
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