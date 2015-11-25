namespace ProjectEuler.LongestCollatz

module Solution =
    open System
    open Checked

    let (|Even|_|) = function
                        | n when n % 2L = 0L -> Some(n)
                        | _ -> None
    let (|Odd|_|) = function
                        | n when (n + 1L) % 2L = 0L -> Some(n)
                        | _ -> None

    let collatz = function 
                    | Even n -> n / 2L
                    | Odd n  -> 3L * n + 1L
                    | _ -> 0L
    /// <summary>
    /// Computes Length of Collatz sequence recursively 
    /// </summary>
    /// <param name="cache">Cache of collatz length</param>
    /// <param name="original">Original Number to be used in cache</param>
    /// <param name="l">Accumulated length</param>
    let rec collatzLength (cache: int64 array) original l = 
            function
            | n when n <= 1L ->
                            //Add value to cache
                            if n < (int64 cache.Length) then
                                cache.[original] <- (l + 1L)
                            (l + 1L)
            //Inside cache:
            | n when n < (int64 cache.Length) 
                        -> 
                            match cache.[int n] with
                            | 0L ->  collatzLength cache original (l + 1L) (collatz n)
                            | l' -> 
                                    cache.[original] <- (l' + l)
                                    (l' + l) 
            //Outside cache:
            | n -> collatzLength cache original (l + 1L) (collatz n) 


    //Rework array to give maxvalue so far / input for maxvalue so far
    let rework a = 
        let maxValue = ref 1L
        let inputMax  = ref 1
        a |> Array.mapi(fun idx v  -> 
                                    if v >= !maxValue then 
                                        maxValue := v
                                        inputMax  := idx
                                        idx
                                    else !inputMax)

    //Find Biggest Collatz
    let biggestCollatz cache n =
            Array.init (n+1) (fun n -> collatzLength cache n 0L (int64 n))
    
    //Dictionnary of Collatz lengths:
    //Allocate space intially to avoid OutOfMemoryException
    let cache = Array.create (5000000 + 1) 0L
    let memo = biggestCollatz cache 5000000 |> rework
    
    ///////////// INPUT OUTPUT //////////////////// 
    ///Reads input in Console, casts to int64
    let consoleReadInt() =  Console.ReadLine() |> int

    let solution() = 
         let n = consoleReadInt() 
         Array.init n (fun i -> consoleReadInt())
         |> Array.map (fun i -> memo.[i])
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