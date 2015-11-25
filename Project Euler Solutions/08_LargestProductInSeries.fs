namespace ProjectEuler.LargestProductInSeries

module Solution =
    open System
    open Checked

    ///Turns a number in an array of digits
    let getDigits (str:string) =
                       str  |> Array.ofSeq
                            |> Array.map (string)
                            //Keep digits along with index
                            |> Array.mapi(fun i d -> (i,d))
    
    ///Looks for a serie of p consecutive digits starting from n
    let getSeries digits i p  = 
                    let l = digits |> Array.length
                    if i + p <=  l then 
                        Some(Array.sub digits i p)
                    else
                        None

    ///Get the largest product of series of p consecutive digits in number n
    let largestProductSeries (n,p) = 
                //Turns to digits
                let digits = getDigits n
                
                digits  |> Array.choose(fun (i,d) -> getSeries digits i p)
                        //Keep only digits
                        |> Array.map(fun arr -> arr |> Array.map snd)
                        //Computes product of series
                        |> Array.map(fun arr -> arr 
                                                    |> Array.map int64
                                                    |> Array.reduce(fun i j -> i * j))
                        |> Array.max

    //Test on big input 1000 characters:
    let input = 
                let rnd = System.Random()
                seq { for i in [|1..100|] do 
                        yield rnd.Next(0,10) |> string
                    }
                    |> Seq.reduce(fun d d' -> d + d')

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
    let consolePrint l = l |> Array.map(fun elem -> printfn "%d" elem)

    ///////////// BUILD SOLUTION ////////////////////
    //Function for the solution:
    let solution (n:int64 option) = 
             match n with 
                | Some(n) 
                    -> 
                        //Cast to int to use List.init:
                        let n' = int n
                        Array.init n' (fun i -> 
                                            //Cast to int to use List.init:
                                            let str = Console.ReadLine() 
                                            let [|l;p|] = str.Split([|' '|]) |> Array.map int
                                            let str = Console.ReadLine()
                                            (str,p) )
                        |> Array.map(fun (str, p) -> largestProductSeries (str, p))
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






