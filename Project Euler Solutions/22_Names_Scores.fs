namespace ProjectEuler

module Names_Scores =
    open System

    let alphabet =
      ['A' .. 'Z']
      |> List.mapi (fun i c -> (c, i + 1))
      |> Map.ofList

    let scoreLetters (input: string) =
        input
        |> Seq.map(fun c -> alphabet.[c] )
        |> Seq.reduce (+)

    ///Reads names list and sort it
    let readNames () =
      let n = Console.ReadLine() |> int
      List.init n (fun _ ->
                Console.ReadLine())
      |> List.sort

    let scoreName names input =
      let pos = List.tryFindIndex(fun name -> name = input ) names
      match pos with
        | None -> 0
        | Some p -> scoreLetters input * (p + 1)

    [<EntryPoint>]
    let main argv =
      let names = readNames()
      let q = Console.ReadLine() |> int
      Array.init q (fun _ ->
          Console.ReadLine()
          |> scoreName names
          |> printfn "%d"
        )
      |> ignore
      0
