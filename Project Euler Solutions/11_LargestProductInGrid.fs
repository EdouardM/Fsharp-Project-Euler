namespace ProjectEuler.LargestProductInGrid

module Array2D =
    
    let ofArray (input: 'a[][]) =
        let N = input.Length
        let M = input.[0].Length

        Array2D.init N M (fun i j -> input.[i].[j])


    ///Gives rows as Array of Array
    let getRows arr = 
        let nbrows = arr |> Array2D.length1
        let nbcols = arr |> Array2D.length2
        [|
            for i in [0 .. nbrows-1] do
            yield [| for j in [0.. nbcols-1] do yield arr.[i,j] |]
        |]

    ///Gives columns as Array of Array
    let getColumns arr =
        let nbrows = arr |> Array2D.length1
        let nbcols = arr |> Array2D.length2
        [|
            for j in [0 .. nbcols-1] do
            yield [| for i in [0.. nbrows-1] do yield arr.[i,j] |]
        |]

    ///Symetric of Array2D on Vertical axis:
    let mirror arr = 
        let nbrows = arr |> Array2D.length1
        let nbcols = arr |> Array2D.length2
        let mirror = Array2D.zeroCreate nbrows nbcols
        for i in [0 .. nbrows-1] do
            for j in [0 .. nbcols-1] do
                mirror.[(nbrows-1) - i, j] <- arr.[i,j]
        mirror

    ///Gives Diagonals Down Right as Array of Array
    let getDiagsDownRight arr =
        let nbrows = arr |> Array2D.length1
        let nbcols = arr |> Array2D.length2
        let arr1 = [|
                        //Offset along rows:
                        for off in [0 .. nbrows-1] do
                            yield [| for i in [0 .. nbrows-1] do
                                        for j in [0..nbcols-1] do
                                        if j = i + off then yield arr.[i,j]  |]
                    |]
        let arr2 = [|
                        //Offset along columns avoid 0 offset
                        for off in [1 .. nbcols-1] do
                            yield [| for i in [0 .. nbrows-1] do
                                        for j in [0..nbcols-1] do
                                        if j + off = i  then yield arr.[i,j]  |]
                    |]
        Array.concat [arr1;arr2]

module Solution =
    open System
    open Checked
    
    let todo = ()

     ///////////// CORE LOGIC ///////////////////
    
    ///Gives All combinations of p consecutive values in an Array as Array of Array
    let getSeries arr p  = 
            let l = arr |> Array.length
            arr |> Array.mapi(fun i v -> if i + p <= l then Some(Array.sub arr i p) else None)
                //Keep only values with Some
                |> Array.choose id

    let getArrays arr = 
        let rows = arr |> Array2D.getRows
        let cols = arr |> Array2D.getColumns
        let diags1 = arr |> Array2D.getDiagsDownRight
        let diags2 = arr |> Array2D.mirror |> Array2D.getDiagsDownRight
        
        Array.concat [rows; cols; diags1; diags2]
         
    let largestProductInGrid p input = 
        //Get all arrays and filter those with enough input:
        let arrays = getArrays input |> Array.filter(fun arr -> arr.Length >= p)

        //Get the series:
        let series = arrays |> Array.collect(fun arr -> getSeries arr p)

        //Compute product and keep max:
        series  |> Array.map(fun arr -> arr |> Array.reduce(fun i j -> i *j))
                |> Array.max
              
    //Test on large input  
    let largeinput = 
        let rand = new System.Random()

        Array2D.init 20 20 (fun i j -> rand.Next(0,100))



    ///////////// INPUT OUTPUT //////////////////// 
    ///Reads input in Console, casts to int64
    let consoleReadArr() =  
                            let str  = Console.ReadLine()
                            str.Split(' ') |> Array.map(fun s -> s |> int)
                            
    ///////////// BUILD SOLUTION ////////////////////
    //Function for the solution:
    let solution ()  = 
                //Cast to int to use List.init:
                Array.init 20 (fun i -> consoleReadArr())   
                |> Array2D.ofArray
                |> largestProductInGrid 4
                |> printfn "%d" 

//Build the program which runs in Console & HackerRank:
module Program = 
    open Solution
    
    //Main program:
    //[<EntryPoint>]
    let main argv = 
        solution()
        0


        