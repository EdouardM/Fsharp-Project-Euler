namespace ProjectEuler.LatticePaths

module Solution =
    open System
    open Checked 

    let rec fact acc =
        function
         | v when v = (bigint 0)    -> acc
         | v when v =  (bigint 1)   -> acc
         | n                        -> fact (acc*n) (n - 1I)

    /// <summary>
    /// Computes factorial function
    /// </summary>
    let factorial n = fact 1I n

    /// <summary>
    /// Computes the binomial coefficient
    /// </summary>
    let binomialCoeff n k = factorial n / (factorial k * factorial (n - k))

    /// <summary>
    /// Computes number of Lattice Path
    /// </summary>
    let latticePaths i j = 
            let nb = binomialCoeff (i + j ) i
            let modulus = bigint (pown 10 9 + 7)
            nb % modulus |> int


    ///////////// INPUT OUTPUT //////////////////// 
    ///Reads input in Console, casts to int64
    let consoleReadInt() =  Console.ReadLine() |> int

    let consoleReadInput() =  
                let input = Console.ReadLine()
                input.Split [|' '|] |> Array.map int
                                    |> Array.map(fun i -> bigint i)

    let solution() = 
         let n = consoleReadInt() 
         Array.init n (fun i -> consoleReadInput())
         |> Array.map (fun [|i;j|] -> latticePaths i j)
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


