namespace ProjectEuler.MaximumPathSum


module Solution =
    open System
    open System.Collections.Generic
    
    type Number = { Position: int; Value: int }
    
    let allPaths (numbers: Number list list) =
        //List of rows = numbers list
        numbers
        |> List.fold(fun acc row ->
                //State: all possible paths through rows
                acc 
                |> List.collect(fun path -> 
                    let num = path |> List.head
                    //For a new row
                    row
                    //Select valid positions
                    |> List.filter(fun n -> n.Position = num.Position || n.Position = num.Position + 1 )
                    //Add position to possible paths 
                    |> List.map(fun n -> n::path))) 
                    //Start state for fold
                    [[ {Position = 0; Value = 0} ]]
        //Reverse and drop start state
        |> List.map (List.rev >> List.tail)
                
    let pathSum = 
        List.map (fun num -> num.Value)
        >> List.reduce (+)
    
    let solution = 
        allPaths 
        >> List.map pathSum
        >> List.max
                
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
        solution l
        |> printfn "%d")
        |> ignore
        0
    
    




(*   let l =
        [ [3]; [7;4]; [2;4;6]; [8;5;9;3]]
        |> List.map (List.mapi(fun i x -> {Position = i; Value = x}))
    
    solution  l 
    
*)