namespace ProjectEuler

module Non_Abundant_Numbers =
    open System

    ///Sum of divisors
    let sumDivisors =
      function
       | 1L -> 1L
       | n ->
          let bound = int64 (sqrt (float n))
          let divs =
             [ for  x in [ 2L..bound ] do
                if n % x = 0L then
                    yield x
                    if n / x <> x then yield n / x  ]
          match divs with
            | []    -> 1L
            | divs  -> divs |> Seq.reduce (+) |> (+) 1L

    let (|Deficient|Perfect|Abundant|) n =
      let s = sumDivisors n
      if n < s then Abundant
      elif n = s then Perfect
      else Deficient

    let isAbundant  = function | Abundant -> true | _ -> false

    let abundant n =
      [1L .. n ]
      |> List.filter isAbundant
      |> Set.ofList

    let dico = abundant 100000L

    let sumAbundant n =
      dico
      |> Set.filter(fun x -> x < n)
      |> Set.exists(fun x -> dico.Contains(n - x))

    let printOut = function | true -> printfn "YES" | false -> printfn "NO"

    [<EntryPoint>]
    let main argv =
      let n = Console.ReadLine() |> int
      Array.init n (fun _ ->
        Console.ReadLine()
        |> int64
        |> sumAbundant
        |> printOut )
      |> ignore
      0
