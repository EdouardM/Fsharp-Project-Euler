namespace ProjectEuler.LongestCollatz

module Memoization =
    open System.Collections.Generic

    let memoize (f:'T -> 'U) = 
        let t = new Dictionary<'T, 'U>()
        (fun n -> if t.ContainsKey(n) then 
                        //printfn "Retrived: key = %A ; value = %A " n t.[n]
                        t.[n]
                  else 
                    let res = f n
                    //printfn "Adding: key = %A;  value = %A" n res
                    t.Add(n, res)
                    res)

module Solution =
    open System
    open Checked
    open Memoization

    let (|Even|_|) = function
                        | n when n % 2L = 0L -> Some(n)
                        | _ -> None
    let (|Odd|_|) = function
                        | n when (n + 1L) % 2L = 0L -> Some(n)
                        | _ -> None
    ///Turns a number in an array of digits
    let collatz = function 
                    | Even n -> n / 2L
                    | Odd n  -> 3L * n + 1L
                    | _ -> 0L

    let collatzSeqLength = 
                (fun numb -> let l = Seq.unfold(fun n -> if n > 1L then Some( n, collatz n) else None) numb                        
                                    |> Seq.length
                             // There is 1 in the seq:
                             l + 1)
                |> memoize

    let longestCollatz n = 
                let lengths = [1L..n] |> List.map (fun i -> i, collatzSeqLength i)
                let (_, max) = lengths |> List.maxBy(fun (i, l) -> l) 
                
                //Get all the input which produces max length
                lengths |> List.filter(fun (i, l) -> l = max)
                        //Keep max input
                        |> List.maxBy(fun (i,l) -> i)
                        |> fst

    ///////////// INPUT OUTPUT //////////////////// 
    ///Reads input in Console, casts to int64
    let consoleReadInt() =  Console.ReadLine() |> int
    let consoleReadInt64() =  Console.ReadLine() |> int64


    let solution() = 
         let n = consoleReadInt() 
         Array.init n (fun i -> consoleReadInt64())
         |> Array.map longestCollatz
         //Get longest collatz
         |> Array.map (fun i -> printfn "%d" i)
         |> ignore

//Build the program which runs in Console & HackerRank:
module Program = 
    open Solution
    
    //Main program:
    //[<EntryPoint>]
    let main argv = 
        solution()
        0
        





