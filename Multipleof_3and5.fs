open System



let filterMultipleSum mult1 mult2 N = Seq.init (N-1) (fun i -> i + 1)  |> Seq.filter(fun elem -> elem % mult1 = 0 || elem % mult2 = 0)
                                                                       |> Seq.sum

let consoleReadInt() =  
                        let str  = Console.ReadLine()
                        try
                                Some(str|> int)
                        with
                                ex-> 
                                    printfn "Bad input: %s" str
                                    None


let consolePrint l = 
                     l |> List.map(fun elem -> match elem with
                                                | Some(n) -> sprintf "%d" n
                                                | None -> "")
                       |> List.map(printfn "%s")

[<EntryPoint>]
let main argv = 
    let nbTest =  consoleReadInt()
    match nbTest with 
            | Some(n) -> 
                        List.init n (fun i -> consoleReadInt()) |> List.map(fun elem -> Option.map (filterMultipleSum 3 5) elem)
                                                                |> consolePrint
                                                                |> ignore
            | None -> printfn "Bad Input"
    0