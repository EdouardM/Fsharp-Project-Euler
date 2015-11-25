namespace ProjectEuler.SummationOfPrimes

module Map = 
    ///Get last values smaller than input in Map with key as int64
    let rec getLast map i = match map |> Map.tryFind(i) with
                                | Some(sum) -> sum
                                | None -> getLast map (i-1L) 


module Solution = 
    open System
    open Checked
    
    let todo = ()
    
    ///////////// CORE LOGIC ///////////////////
    let noFactorOf n s = s |> Seq.exists(fun i -> n % i = 0L ) |> not

     ///Tells if a number is a Prime number
    let isPrime n = 
                    let bound = int64 (sqrt(float n))
                    seq{2L..bound} |> noFactorOf n
    
    ///Active Pattern Prime numbers
    let (|Prime|_|) n = if isPrime n then 
                            Some(n) 
                        else 
                            None 
    ///Computes primes and sum
    let rec nextPrime (i, sum) = match (i + 1L) with
                                    | Prime n -> (n, sum + n)
                                    | _ -> nextPrime ((i + 1L), sum)

    //Prime numbers memorized Map
    let primes = Seq.unfold (fun (i,sum) -> if i <= 1000000L then Some((i,sum), nextPrime (i,sum)) else None) (2L,2L)
                 |> Map.ofSeq

    ///Computes the sum of primes below n
    let sumOfPrimes n = Map.getLast primes n
                          
    ///Find the sum of all primes not greater than n
    let summmationOfPrimes = function
                                | 1L -> 1L
                                | n when n > 1L -> sumOfPrimes n
                                | _ -> 0L

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
                        Array.init n' (fun i -> consoleReadInt())   
                        |> Array.map(fun elem -> Option.map sumOfPrimes elem)
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