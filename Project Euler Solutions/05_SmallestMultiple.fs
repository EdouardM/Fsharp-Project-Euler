namespace ProjectEuler.SmallestMultiple

module Solution =
    open System

    //List of primes less than 40
    let primes = [2;3;5;7;11;13;17;19;23;29;31;37] |> List.map int64

    //Returns list of primes for a number:
    let primeFactorize = 
            let rec factorize numb acc = 
                let primeDivisor = primes |> List.tryFind(fun p -> numb % p = 0L)
                match primeDivisor with
                                | Some(p) -> 
                                            let newAcc = acc@[p]
                                            let newNumb = (numb / p)
                                            factorize  newNumb newAcc
                                | None ->   acc

            (fun numb -> factorize numb [])

    //Format factorization:
    let formatFactorization (l: int64 list) = 
        let set = l |> Set.ofList
        [| for p in set do
            let puissance = l |> List.filter(fun i -> i = p) |> List.length   
            yield (float p , float puissance)
        |]

    //New formula of ppcm based on prime factorization formatted:
    let ppcm arr =
        seq { 
                           for (p,pow) in arr do
                               let pArr = arr |> Seq.filter (fun (i,j) -> i = p)
                               let pow' = pArr |> Seq.map snd |> Seq.max 
                               yield (p, pow')
            } 
            //Remove duplicates 
            |> Set.ofSeq
            //Calculate
            |> Set.fold(fun acc (p,pow) -> acc * Math.Pow(p,pow) ) 1.0
            |> int64

    let smallestMultiple n = [1L..n] |> Seq.collect ( primeFactorize >> formatFactorization)
                                     |> ppcm
          
          
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
                |> Array.map(fun elem -> Option.map smallestMultiple elem)
                |> consolePrint
                |> ignore
        | None -> printfn "Bad Input"


//Test the solution with the test cases provided in problem description:
module Testcases =
    open Solution
    open Xunit
        
    [<Fact>]
    let ``1 to 3 smallest multiple is 6``() = 
         try 
            Assert.Equal(6L, smallestMultiple 3L)
            printfn "Test OK"
         with
            ex -> printfn "Test KO: %s" ex.Message
    [<Fact>]
    let ``1 to 10 smallest multiple is 2520``() = 
        try 
            Assert.Equal(2520L, smallestMultiple 10L)
            printfn "Test OK"
        with
            ex -> printfn "Test KO: %s" ex.Message

//Build the program which runs in Console & HackerRank:
module Program = 
    open Solution
    
    //Main program:
    //[<EntryPoint>]
    let main argv = 
        let nbTest =  consoleReadInt()
        solution nbTest
        0
                    