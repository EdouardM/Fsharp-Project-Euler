namespace ProjectEuler.Multipleof3and5 
module Solution = 
    open System

    ///////////// CORE LOGIC ////////////////////

    //FIRST SOLUTION:
    //Generate a sequence filtered with multiple of mult 1 and mult2 and sum elements:
    //Computation complexity = O(n)
    let calculateSeq N =   seq{
                                        for i in [1..(N-1)] do
                                            match i with 
                                                | i when i % 3 = 0 -> yield int64 i
                                                | i when i% 5 = 0 -> yield int64 i
                                                | _ -> ()
                            } |> Seq.sum 

    //SECOND SOLUTION:
    //Sum of N integers 1 to N, int64 to deal with large numbers:
    let sum1toN (N:int64) = N * ( N + 1L ) / 2L

    //Get the biggest multiple of mult in N:
    let biggestMultiple mult (N:int64) = N - (N % mult)

    (* Calculate Sum: 1 operation no matter input e.g complexity O(1)
        Steps for sum of 3: 
            - Get the biggest multiple of 3 in N : mult
            - Store N' = Divide mult by 3
            - Calculate Sum of integer from 1 to N'
            - Multiply by 3
            - Put 3 as parameter of function sumcalc
    
        Steps for sum of 5: 
            Same sumcalc with 5 as input
    *)
    let sumofMult mult N =
            //Exclude N > N-1
            let m = biggestMultiple mult (N-1L)
            let n = m / mult
            let s = sum1toN n
            s * mult

    let sumofMultList multList N = 
            match multList with
                |[m1;m2] -> 
                            let s1= [m1;m2] |> List.map(fun elem -> sumofMult elem N)
                                            |> List.reduce(fun acc elem -> acc + elem)
                            let s2 = sumofMult (m1 * m2) N
                            s1 - s2
                | _ -> 
                        printfn "Wrong Multiple List input: %A" multList
                        0L


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
                            Array.init n' (fun i -> consoleReadInt()) |> Array.map(fun elem -> Option.map (sumofMultList [3L;5L]) elem)
                                                                      |> consolePrint
                                                                      |> ignore
                | None -> printfn "Bad Input"

//Test the solution with the test cases provided in problem description:
module Testcases =
    open Solution
    open Xunit
        
    //(I) 10 => (O) 23
    [<Fact>]
    let ``sumofMultList 10 gives 23``() = 
        Assert.Equal(sumofMultList [3L; 5L] 10L, 23L)
    
    //(I) 100 => (O) 2318
    [<Fact>]
    let ``sumofMultList 100 gives 2318``() = 
        Assert.Equal(sumofMultList [3L; 5L] 100L, 2318L)

    //(I) 10^7 => Should end fast < 6secs
    //(I) 10^8 => Should end fast < 6secs
    //(I) 10^9 => Should end fast < 6secs


//Build the program which runs in Console & HackerRank:
module Program = 
    open Solution
    
    //Main program:
    //[<EntryPoint>]
    let main argv = 
        let nbTest =  consoleReadInt()
        solution nbTest
        0