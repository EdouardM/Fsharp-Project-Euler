namespace ProjectEuler


module Factorial_Digit_Sum =
    open System

    let factorial =
      function
        | 0 -> bigint 1
        | n -> [1..n] |> List.map (fun e -> bigint e) |> List.reduce (*)

    let solution =
      factorial
      >> string
      >> Seq.map (string >> int)
      >> Seq.reduce (+)

    [<EntryPoint>]
    let main argv =
      let t = Console.ReadLine() |> int
      Array.init t (fun _ ->
        Console.ReadLine()
        |> int
        |> solution
        |> printfn "%d"
      )
      |> ignore
      0
