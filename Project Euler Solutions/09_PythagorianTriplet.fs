namespace ProjectEuler.PythargorianTriplet

module Solution =
    open System
    open Checked

    ///Check for Pythagorian Triplet
    let isPythagorianTriplet a b c = 
                a * a + b * b = c * c

    ///Active Pattern for Pythagorian triplet
    let (|PythagorianTriplet|_|) (a,b,c) = 
                    if isPythagorianTriplet a b c then 
                        Some (a, b, c) 
                    else None

    ///Find Pythagorian Triplet for a number n => Slow O(n^2)
    let findTriplet n =
            [|for i in [|1..(n-1)|] do
                for j in [|i+1..n|] do
                    match (i,j,n) with
                        | PythagorianTriplet (a,b,c) -> yield (a,b,c)
                        | _ -> () 
            |]

    ///Find Triplet for a number => Fast O(n)
    let findTriplet' n =
            [|for i in [|1..(n-1)|] do
                let j = (n*n - 2*n*i) /(2*n - 2*i)
                if j > i then yield (i,j,n-i-j)
            |]
    ///Function to find special Pythagorian triplet a,b,c = n and picks max, -1 if none
    let specialTriplet n = 
            try
                [|1..n|] |> Array.collect findTriplet
                         |> Array.filter(fun (i,j,k) -> i + j + k = n)
                         |> Array.map(fun (i,j,k) -> i * j * k)
                         |> Array.max
            with
                :? System.ArgumentException -> - 1 


    let specialTriplet' n =
            try
                findTriplet' n   |> Array.filter(fun (i,j,k) -> isPythagorianTriplet i j k)
                                 |> Array.map(fun (i,j,k) -> i * j * k)
                                 |> Array.max
            with
                :? System.ArgumentException -> - 1 
       

 ///////////// INPUT OUTPUT //////////////////// 
    ///Reads input in Console, casts to int64
    let consoleReadInt() =  
                            let str  = Console.ReadLine()
                            try
                                    Some(str|> int)
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
    let solution (n:int option) = 
             match n with 
                | Some(n) 
                    -> 
                        //Cast to int to use List.init:
                        let n' = int n
                        Array.init n' (fun i -> consoleReadInt())   
                        |> Array.map(fun elem -> Option.map specialTriplet' elem)
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