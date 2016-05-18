namespace ProjectEuler.MaximumPathSum


module Solution =
    open System
    
    type Number = { Position: int; Value: int }
    
    let rec loop numbers acc =
        match numbers with 
            | [] -> acc 
            | l::ls -> 
                let pos, sum = acc
                let num = 
                    l
                    |> List.filter(fun n -> n.Position = pos || n.Position = pos + 1) 
                    |> List.maxBy(fun n -> 
                        let newAcc = n.Position, n.Value + sum
                        loop ls newAcc |> snd)
                let newAcc = num.Position, num.Value + sum
                printfn "newAcc: pos:%d // sum: %d" num.Position (snd newAcc)
                loop ls newAcc
                
                
    let readInput() =
        //Number of rows of triangle
        let n = Console.ReadLine() |> int 
        //Read Pyramid numbers
        List.init n (fun _ ->
            Console.ReadLine().Split [|' '|] 
            |> Array.map int
            |> Array.mapi(fun i x -> {Position = i; Value = x})
            |> Array.toList)

    [<EntryPoint>]
    let main argv =
        //Number of test cases
        let t = Console.ReadLine() |> int
        Array.init t (fun _ ->
        let l = readInput()
        loop l (0, 0)
        |> snd
        |> printfn "%d")
        |> ignore
        0
    
    (*
    let l =
        [ [3]; [7;4]; [2;4;6]; [8;5;9;3]]
        |> List.map (List.mapi(fun i x -> {Position = i; Value = x}))
    
    loop l (0,0)
    *)