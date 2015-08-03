namespace ProjectEuler.HighlyDivisibleTriangleNumber


module Map = 
    ///Get last values smaller than input in Map with key as int64
    let rec getLast map i = match map |> Map.tryFind(i) with
                                | Some(sum) -> sum
                                | None -> getLast map (i+1) 

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

module PrimeFactorization = 
    open System
    open System.Collections.Generic
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
    let rec nextPrime i = match (i + 1L) with
                                    | Prime n -> n
                                    | _ -> nextPrime (i + 1L)

    //Prime numbers memorized List => Solution from problem 7
    let primes = Seq.unfold (fun i -> if i <= 1000000L then Some(i , nextPrime i) else None) (2L)
                 |> List.ofSeq

   
    //Returns list of primes for a number => Solution from problem 05
    let factorize =
            let t = new Dictionary<int64, int64 list>()
            
            let rec loop numb acc =
                    match numb with
                        | 1L -> acc
                        | n ->
                            let bound = int64 (sqrt(float numb))
                            let p = primes |> List.find(fun p -> numb % p = 0L)
                            let newAcc = acc@[p]
                            let newNumb = (numb / p)
                            loop newNumb newAcc
            (fun numb -> loop numb [])

    //Format factorization:
    let formatFactorization (l: int64 list) = 
        let set = l |> Set.ofList
        [| for p in set do
            let puissance = l |> List.filter(fun i -> i = p) |> List.length   
            yield (p , puissance)
        |]

    //Prime Factorization function:
    let primefactorization = factorize >> formatFactorization

module Solution =
    open System
    open Checked
    open PrimeFactorization
    open Memoization

    let todo = ()
    
    ///////////// CORE LOGIC ///////////////////
    ///Tells if a number is a Triangle number
    let cumulsum n = n, n * (n + 1L) / 2L

    //Gives number of factors:
    let nbfactors  = (fun n -> 
                        primefactorization n 
                        |> Array.fold(fun acc (p, nbfact)  -> (nbfact + 1) * acc) 1)
                      |> memoize
   
   
    let mult i j = i * j 

    //Get number of factors for triangle number:
    // triangle = n * (n + 1) / 2
    //n and (n+1) are coprimes so are n/2 & (n+1) if n even, (n+1)/2 & n if n odd
    let nbfactorsTriangle = 
                (function
                    | n when n % 2L = 0L -> 
                            let (i, cumul) = cumulsum n 
                            let nbfact = nbfactors (n + 1L) |> mult <| nbfactors (n / 2L)
                            i, cumul, nbfact
                    | n ->
                            let (i, cumul) = cumulsum n 
                            let nbfact = nbfactors ((n + 1L) / 2L) |> mult <| nbfactors n 
                            i, cumul, nbfact)

    let triangles = 
                let max = ref (1L, 1L, 1)
                let list = Seq.unfold (fun (n, cumul, nbfact) -> if 
                                                                    nbfact <= 1000 then Some((n, cumul, nbfact) , nbfactorsTriangle (n + 1L)) 
                                                                 else 
                                                                    max:= (n,cumul,nbfact)
                                                                    None) (1L, 1L, 1)
                            |> Seq.map(fun (n, cumul, nbfact) -> (nbfact, cumul))
                            |> List.ofSeq          
    
                let _, cumulmax, nbfactmax= !max
                list@[(nbfactmax, cumulmax)]

    
    
    let mapTriangle = [|
                        for i in [1..1000] do
                            let res = triangles |> Seq.tryFind(fun (nbfact, c) -> nbfact > i)
                            match res with
                            | Some (nbfact,cumul) -> yield i, cumul
                            | None -> ()
                      |]|> Map.ofArray

    ///////////// INPUT OUTPUT //////////////////// 
    ///Reads input in Console, casts to int64
    let consoleRead() =  
                            let str  = Console.ReadLine()
                            str |> int
                            
    ///////////// BUILD SOLUTION ////////////////////
    //Function for the solution:
    let solution ()  = 
                let t = consoleRead()
                Array.init t (fun i -> consoleRead())
                |> Array.map (fun i -> mapTriangle.[i])
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