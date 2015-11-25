namespace ProjectEuler.FibonacciEven

module Solution =
    open System

    //Calculate fibonacci numbers:
    //Starting with 1 and 2:
    let rec fibo = function
                | 0 -> 1
                | 1 -> 2
                | n when n > 0 -> fibo (n-1) + fibo (n-2)
                | _ -> 0

    //Build a sequence of fibonacci numbers:
    let fiboUtil = Seq.unfold (fun num -> if (num <=10) then Some(string(num), num + 1) else None) 0


    






