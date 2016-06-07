namespace ProjectEuler

module Amicable_Numbers =
    open System
    ///////////// CORE LOGIC ///////////////////
    let noFactorOf n s = s |> Seq.exists(fun i -> n % i = 0L ) |> not

    ///Tells if a number is a Prime number
    let isPrime n =
     if n = 1L then
      false
     else
      let bound = int64 (sqrt(float n))
      seq{2L..bound} |> noFactorOf n

    ///Active Pattern Prime numbers
    let (|Prime|_|) n = if isPrime n then Some n else None

    ///Sum of divisors
    let sumDivisors =
      function
       | Prime n -> 1L
       | n ->
          let bound = int64 (sqrt (float n))
          seq { for  x in [ 2L..bound ] do
                if n % x = 0L then
                    yield x
                    if n / x <> x then yield n / x }
          |> Seq.reduce (+)
          |> (+) 1L
        | 1L -> 1L

    let memoSums n =
      Seq.unfold(fun i ->
                if i > n then None
                else Some ( (i, sumDivisors i ) , i + 1L) ) 2L
      |> Seq.filter(fun (_, sum ) -> sum <> 1L)
      |> Map.ofSeq

    let dico = memoSums 100000L

    let amicables =
      let pairs = dico |> Map.toList
      [ for (k, sum) in pairs do
          if k <> sum && dico.ContainsKey(sum) then
            if dico.[sum] = k then yield (k, sum) ]
      |> List.map fst

    let solution n =
          let res = amicables |> List.filter(fun x -> x <= n)
          match res with
            | [] -> 0L
            | l -> List.reduce (+) l

    [<EntryPoint>]
    let main argv =
      let t = Console.ReadLine() |> int
      Array.init t (fun _ ->
        let n = Console.ReadLine() |> int64
        solution n
        |> printfn "%d"
        )
      |> ignore
      0
