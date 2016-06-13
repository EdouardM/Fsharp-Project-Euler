namespace ProjectEuler


//https://www.hackerrank.com/contests/projecteuler/challenges/euler024?h_r=internal-search
module Lexicographic_Permutations =
    open System
    open System.Collections.Generic

    //Solution with factoradic numbers
    //https://en.wikipedia.org/wiki/Factorial_number_system
    let toFactoradic (n: int64) digits =
      let rec loop acc n divisor digits =
        if n / divisor = 0L then
          let newAcc = (n % divisor) :: acc
          if divisor = digits then newAcc
          else
            loop newAcc (n / divisor) (divisor + 1L) digits
        else
          let newAcc = (n % divisor) :: acc
          loop newAcc (n / divisor) (divisor + 1L) digits
      loop [] n 1L digits

    toFactoradic 0L 3L |> ignore

    let rec fact = function
                    | 0L -> 1L
                    | n -> n * (fact (n - 1L))

    let nthPermutation arr digits n =
      let factor = toFactoradic n digits
      let rec loop acc (arr: 'a []) (l:int64 list) =
        match l with
          | [] -> acc |> List.rev
          | x::xs ->
              let pos = int x
              let newAcc = arr.[pos] :: acc
              let newArr = Array.except [ arr.[pos] ] arr
              loop newAcc newArr xs
      loop [] arr factor

    let solution input n =
      let digits = input |> Array.length |> int64
      if n >= 1L && n <= (fact digits - 1L) then
        nthPermutation input digits n
      elif n = 0L then
        Array.toList input
      else
        failwith "Invalid input: n must <= max count of permutations"

    //Test: should equal [4; 0; 6; 2; 1; 3; 5]
    solution [|0..6|] 2982L |> ignore

    //[<EntryPoint>]
    let main argv =
      let arr = "abcdefghijklm" |> Seq.map string |> Array.ofSeq
      let t = Console.ReadLine() |> int
      Array.init t (fun _ ->
          Console.ReadLine()
          |> int64
          |> (fun x -> x - 1L)
          |> solution arr
          |> String.concat ""
          |> printfn "%s"
        )
        |> ignore
      0

(*
      //Follow guidelines here: http://www.geeksforgeeks.org/lexicographic-permutations-of-string/

    ///find the rightmost character "pivot", which is smaller than its next character
    let findPivot: (int * string) [] -> (int * string) option =
        Array.pairwise
        >> Array.filter (fun ((_, elem1), (_, elem2)) -> elem1 < elem2)
        >> Array.tryLast
        >> Option.map fst

    ///find ceiling is the smallest character on right of ‘first character’, which is greater than ‘first character’.
    let findCeiling (pos, c) =
        Array.filter (fun (i, elem) -> elem > c && i > pos )
        >> Array.minBy snd

    ///swap two character in an array
    let swap (pos, c) (pos2, d) (arr: (int * 'b) []) =
        arr.[pos] <- (pos, d)
        arr.[pos2] <-(pos2, c)
        arr

    ///Sort the substring after the original index of ‘first character’.
    let sortSubarray (pos, c) arr =
        let arr1, arr2 = Array.partition(fun (i, elem) -> i <= pos ) arr
        Array.concat [arr1; (Array.sortBy snd arr2)]

    //Test
    [|"a"; "c"; "b"|]
    |> Array.mapi (fun i elem -> (i, elem))
    |> swap (0, "a") (2, "b")
    |> sortSubarray (0, "a")
    |> ignore


    let memo = new Dictionary<int, string []>()
    let arr2 = "abcdefghijklm" |> Seq.map string |> Array.ofSeq
    memo.Add(1, arr)

    ///Find next permutation in lexicographic order
    let nextPerm (i, (arr: string [])) =
        if memo.ContainsKey(i + 1) then
          memo.[i + 1] |> Some
        else
          let arr = Array.mapi (fun i elem -> (i, elem)) arr
          match findPivot arr with
            | Some (pos, pivot) ->
              let (pos2, ceiling) = findCeiling (pos, pivot) arr
              let res =
                arr
                |> swap (pos, pivot) (pos2, ceiling)
                |> sortSubarray (pos, pivot)
                |> Array.map snd
              memo.Add(i + 1, res)
              Some res
            | None -> None

    ///Distributes value in list
    let distrib e l =
      let rec loop pre post =
        seq { match post with
              | [] -> yield ( l @ [e] )
              | h::t ->
                yield (List.rev pre @ [e] @ post)
                yield! loop (h::pre) t
            }
      loop [] l

    ///Returns all permutations of one list in a list
    let rec permute =
      function
        | [] -> Seq.singleton []
        | h::t ->
            Seq.collect (distrib h) (permute t)

    let perms =
        "abcdefghijklm"
        |> Seq.map string
        |> List.ofSeq
        |> permute
        |> Seq.map (String.concat "")
        |> Seq.mapi (fun i perm -> (i + 1, perm))
*)
