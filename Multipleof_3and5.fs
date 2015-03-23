open System

let filterMultipleSum mult1 mult2 N = 
                            seq{
                                    for i in [1..(N-1)] do
                                        if i % mult1 = 0 || i% mult2 = 0 then
                                            yield float i
                                } |> Seq.sum 

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
                                                | Some(n) -> sprintf "%.0f" n
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