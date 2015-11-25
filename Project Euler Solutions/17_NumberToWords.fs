namespace ProjectEuler.NumberToWords


module Solution =
    open System

    let toWord n = 
        try
            match n with
             | 1L -> "One" | 2L -> "Two" | 3L -> "Three" | 4L -> "Four"
             | 5L -> "Five" | 6L -> "Six" | 7L -> "Seven" | 8L -> "Eight" | 9L -> "Nine"
             | 10L -> "Ten" | 11L -> "Eleven" | 12L -> "Twelve" | 13L -> "Thirteen"
             | 14L -> "Fourteen" | 15L -> "Fifteen" | 16L -> "Sixteen" | 17L -> "Seventeen"
             | 18L -> "Eighteen" | 19L -> "Nineteen" | 20L -> "Twenty" | 30L -> "Thirty"
             | 40L -> "Forty" | 50L -> "Fifty" | 60L -> "Sixty" | 70L -> "Seventy"
             | 80L -> "Eighty" | 90L -> "Ninety"
             | 100L -> "Hundred" | 1000L -> "Thousand"
             | 1000000L -> "Million"
             | 1000000000L -> "Billion"
             | 1000000000000L -> "Trillion"
        with
            ex -> sprintf "Unexpected input: %d" n

    
    let (|Upto20|Tens|Hundreds|Thousands|Millions|Billions|Trillions|) =
        function
            | n when n >= 1L && n <= 20L -> Upto20 n
            | n when n < 100L -> Tens n
            | n when n >= 100L && n < 1000L -> Hundreds n
            | n when n >= 1000L && n < 1000000L -> Thousands n
            | n when n >= 1000000L && n < 1000000000L -> Millions n
            | n when n >= 1000000000L && n < 1000000000000L -> Billions n
            | n -> Trillions n

    let rec convert n =
        let helper boundary n =
            let modulo = n % boundary
            let units = n / boundary
            if modulo > 0L then 
                (convert units) + " " + (boundary |> toWord) + " " + (convert modulo)
            else
                (convert units) + " " + (boundary |> toWord)

        match n with
        | Upto20 n         -> toWord n
        | Tens n
            ->
            let modulo = n % 10L
            let remainder = modulo
            let tens = (n - remainder) |> toWord
            if remainder > 0L then 
                tens + " " + (remainder |> toWord)
            else tens
        | Hundreds n    -> helper 100L n
        | Thousands n   -> helper 1000L n
        | Millions n    -> helper 1000000L n
        | Billions n    -> helper 1000000000L n
        | Trillions n   -> helper 1000000000000L n
        

    ///////////// INPUT OUTPUT //////////////////// 
    ///Reads input in Console, casts to int64
    let consoleReadInt() =  Console.ReadLine() |> int

    let solution() = 
         let n = consoleReadInt() 
         Array.init n (fun i -> Console.ReadLine() |> int64)
         |> Array.map (fun i -> if i = 0L then "Zero" else (convert i) )
         |> Array.map (fun s -> printfn "%s" s)
         |> ignore    

//Build the program which runs in Console & HackerRank:
module Program = 
    open Solution
    
    //Main program:
    //[<EntryPoint>]
    let main argv = 
        solution()
        0
