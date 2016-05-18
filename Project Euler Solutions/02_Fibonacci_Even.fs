namespace ProjectEuler.FibonacciEven

module Solution =
    open System

    ///////////// CORE LOGIC ////////////////////
    //Calculate fibonacci numbers:
    ///Selects even numbers:
    let isEven = function
                    | n when n % 2L = 0L -> true
                    | _ -> false
 
    ///Builds a sequence of fibonacci numbers:
    let fiboUtil n = Seq.unfold (fun (n0, n1) -> if (n0 > n) then None else Some(n0 , (n1 , n0 + n1))) (1L,1L)

    ///Sums Fibonacci even numbers under one limit (parameter)
    let evenFibo  = fiboUtil >> Seq.filter isEven >> Seq.sum

     ///////////// INPUT OUTPUT ////////////////////
    //Read input in Console, cast to int64:
    let consoleReadInt() =  let str  = Console.ReadLine()
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
                            Array.init n' (fun i -> consoleReadInt()) |> Array.map(fun elem -> Option.map evenFibo elem)
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
