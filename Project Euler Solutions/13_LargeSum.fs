namespace ProjectEuler.LargeSum

module Solution =
    open System
    open Checked

    ///Turns a number in an array of digits
    let getDigits (str:string) =
                       str  |> Array.ofSeq
                            //convert char to string then int
                            |> Array.map (string >> int)
    
    let sumArray arr =  
                        let memo = ref 0
                        let arr' = [| for d in arr do
                                        let res = d + !memo
                                        match res with
                                            | r when r < 10 ->
                                                    memo := 0
                                                    yield r
                                            | r' ->
                                                    let res' = r' - 10
                                                    memo := 1
                                                    yield res' |]
                        //If memo <> 0 then array is bigger:
                        match !memo with
                            | 0 -> arr'
                            | m -> Array.concat [arr'; [|m|]]

    let augment offset arr = 
                        let prefix = Array.init offset (fun i -> 0)
                        Array.concat [prefix ; arr]
    
    //To have arrays of same length
    let arrangeLength arr1 arr2 = 
                let l1 = arr1 |> Array.length
                let l2 = arr2 |> Array.length
                if l1 > l2 then
                    arr1, augment (l1 - l2) arr2
                elif l1 < l2 then 
                    augment (l2 - l1) arr1, arr2
                else
                    arr1, arr2


    let sum arr1 arr2 = 
                        let arr1,arr2 = arrangeLength arr1 arr2
                        Array.zip arr1 arr2
                        |> Array.map (fun (i,j) -> i + j)
                        // Sum in the good order
                        |> Array.rev
                        |> sumArray
                        |> Array.rev
      
    ///////////// INPUT OUTPUT //////////////////// 
    ///Reads input in Console, casts to int
    let consoleReadInt() =  Console.ReadLine() |> int

    let solution() = 
         let n = consoleReadInt() 
         let bigSum = Array.init n (fun i -> Console.ReadLine())
                      |> Array.map getDigits
                      //Sum big numbers:
                      |> Array.reduce(fun arr1 arr2 -> sum arr1 arr2)
        

         //Pick only 10 first digits:
         bigSum.[0..9] |> Array.map string
                       |> Array.fold(fun d d' -> d + d') ""
                       //print to console
                       |> printfn "%s"

//Build the program which runs in Console & HackerRank:
module Program = 
    open Solution
    
    //Main program:
    //[<EntryPoint>]
    let main argv = 
        solution()
        0




                        