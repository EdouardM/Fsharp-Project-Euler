namespace ProjectEuler

module ndigits_Fibonacci =
    open System
    open Checked

    let gold = (1. + sqrt 5.) / 2.

    ///Based on functional expression of Fib numbers:
    ///https://fr.wikipedia.org/wiki/Suite_de_Fibonacci#Algorithme_logarithmique
    let fib n =
      1. / (sqrt 5.) * (pown gold n)
      |> bigint

    ///Find index of first Fib number bigger than x
    let findFib x =
      (log (sqrt 5.) + x ) /  log gold
      //round to upper int
      |> ceil

    [<EntryPoint>]
    let main argv =
      let t = Console.ReadLine() |> int
      Array.init t (fun _ ->
        Console.ReadLine()
        |> float
        |> (fun x -> log 10. * (x - 1.) )
        |> findFib
        |> printfn "%.0f"
        )
      |> ignore
      0
