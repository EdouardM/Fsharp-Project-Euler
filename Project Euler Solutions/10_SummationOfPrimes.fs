namespace ProjectEuler.SummationOfPrimes

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
    let (|Prime|_|) n = if isPrime n then 
                            Some(n) 
                        else 
                            None 

    let rec nextPrime i = match (i + 1L) with
                            | Prime n -> n
                            | _ -> nextPrime (i + 1L)

    //Prime numbers memorized list
    let primes = Seq.unfold (fun i -> if i <= 1000000L then Some(i, nextPrime i) else None) 2L
                 |> Seq.toList
    
    ///Computes the sum of primes below n
    let sumOfPrimes n = primes  |> Seq.takeWhile (fun p -> p <= n)
                                |> Seq.reduce (fun p p' -> p + p')

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