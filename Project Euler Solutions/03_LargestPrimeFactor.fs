namespace ProjectEuler.LargestPrimeFactor

module Solution =
    open System
    open Checked 
    ///////////// CORE LOGIC ///////////////////

    let noFactorOf n s = s |> Seq.exists(fun i -> n % i = 0L ) |> not

    ///Tells if a number is a Prime number
    let isPrime n = 
                    let bound = int64 (sqrt(float n))
                    seq{2L..bound} |> noFactorOf n

    ///Active Pattern Prime numbers
    let (|Prime|_|) n = if isPrime n then Some(n) else None 

    ///Picks the max prime factor or the number itself if it is prime
    let largestPrimeFactor = function 
                                | Prime n -> n
                                | n -> 
                                    let bound = int64 (sqrt(float n))
                                    [2L..bound]
                                    |> Seq.filter(fun p -> n % p = 0L)
                                    // To have all factors : keep both
                                    |> Seq.collect(fun p -> [p ; n/p])
                                    |> Seq.filter isPrime
                                    |> Seq.max
    
    ///////////// INPUT OUTPUT //////////////////// 
    ///Reads input in Console, casts to int64
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
                | Some(n) 
                    -> 
                        //Cast to int to use List.init:
                        let n' = int n
                        Array.init n' (fun i -> consoleReadInt())   |> Array.map(fun elem -> Option.map largestPrimeFactor elem)
                                                                    |> consolePrint
                                                                    |> ignore
                | None -> printfn "Bad Input"

//Test the solution with the test cases provided in problem description:
module Testcases =
    open Solution
    open Xunit
        
    [<Fact>]
    let ``13195 largest prime factor is 29``() = 
        Assert.Equal(29L, largestPrimeFactor 13195L)
    
    [<Fact>]
    let ``10 largest prime factor is 5``() = 
        Assert.Equal(5L, largestPrimeFactor 10L)

    [<Fact>]
    let ``17 largest prime factor is 17``() = 
        Assert.Equal(17L, largestPrimeFactor 17L)

//Build the program which runs in Console & HackerRank:
module Program = 
    open Solution
    
    //Main program:
    [<EntryPoint>]
    let main argv = 
        let nbTest =  consoleReadInt()
        solution nbTest
        0