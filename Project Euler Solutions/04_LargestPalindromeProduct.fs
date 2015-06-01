namespace ProjectEuler.LargestPalindromeProduct

module Solution =
    open System

    ///Pattern for palindrome
    let (|Palindrome|_|) (str:string) = 
                                        let arr = str.ToCharArray()
                                        if arr = (arr |> Array.rev) then Some(str) else None

    let isPalindrome (n:int64) = match (string n) with
                                    | Palindrome s -> true
                                    | _ -> false

    ///Tells if a number is a product of 2 3 digits numbers:
    let has3digitsFactors n = 
        [100L..999L] 
        |> Seq.exists(fun p -> n % p = 0L && n/p  >= 100L && n/p <= 999L)
    
    ///Sepcial operator to combine boolean functions:      
    let (&&&) f g x = f x && g x 
       
    ///Picks the max palindrome less than n
    let largestPalindrome n =
                            let bound = min (999L * 999L) n
                            [101101L..bound]
                            |> Seq.filter (isPalindrome &&& has3digitsFactors)
                            |> Seq.max


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
                |> Array.map(fun elem -> Option.map largestPalindrome elem)
                |> consolePrint
                |> ignore
        | None -> printfn "Bad Input"


//Test the solution with the test cases provided in problem description:
module Testcases =
    open Solution
    open Xunit
        
    [<Fact>]
    let ``101110 largest palindrome is 101101``() = 
         try 
            Assert.Equal(101101L, largestPalindrome 101110L)
            printfn "Test OK"
         with
            ex -> printfn "Test KO: %s" ex.Message
    
    [<Fact>]
    let ``800000 largest palindrome is 793397``() = 
        try 
            Assert.Equal(793397L, largestPalindrome 800000L)
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