namespace ProjectEuler


module Counting_Sundays =
    open System

    type WeekDays = | Monday  = 1 | Tuesday = 2 | Wednesday = 3 | Thursday = 4 | Friday = 5
                    | Saturday = 6 | Sunday = 7

    type Months =   | January = 1   | February = 2  | March = 3     | April = 4
                    | May = 5       | June = 6      | July = 7      | August = 8
                    | September = 9 | October = 10  | November = 11 | December = 12
    let toMonth =  enum<Months>

    let equalsOn f x (yobj: obj) =
      match yobj with
        | :?'T as y -> (f x = f y)
        | _ -> false

    let hashOn f x = hash (f x)

    [<CustomEquality;CustomComparison>]
    type Date =
        { Day : int; Weekday: WeekDays; Month: Months; Year: int64}
          static member Stamp d = d.Day, d.Month, d.Year
          override x.Equals other = equalsOn Date.Stamp x other
          override x.GetHashCode() = hashOn Date.Stamp x
          interface System.IComparable with
            member this.CompareTo other =
              match other with
              | :? Date as other ->
                //Campare Year first
                match this.Year.CompareTo(other.Year) with
                                  //Campare Month
                                  | 0 -> match this.Month.CompareTo(other.Month) with
                                          //Campare Day
                                          | 0 -> this.Day.CompareTo(other.Day)
                                          | n -> n
                                  | n -> n
              | _ -> invalidArg "other" "cannot compare"


    type ZellerMonths =
                    | March = 3   | April = 4   | May = 5         | June = 6
                    | July = 7    | August = 8  | September = 9   | October = 10
                    | November = 11   | December = 12             | January = 13
                    | February = 14

    type ZellerDays =
                    | Monday = 2  | Tuesday = 3  | Wednesday = 4  | Thursday = 5
                    | Friday = 6  | Saturday = 0 | Sunday = 1

    let toZellerMonths =
              function
              | Months.January -> ZellerMonths.January
              | Months.February -> ZellerMonths.February
              | Months.March -> ZellerMonths.March
              | Months.April -> ZellerMonths.April
              | Months.May  -> ZellerMonths.May
              | Months.June -> ZellerMonths.June
              | Months.July -> ZellerMonths.July
              | Months.August -> ZellerMonths.August
              | Months.September -> ZellerMonths.September
              | Months.October  -> ZellerMonths.October
              | Months.November -> ZellerMonths.November
              | Months.December -> ZellerMonths.December
              | _ -> invalidArg "month" "invalid month"

    let toWeekDays =
            function
            | ZellerDays.Monday -> WeekDays.Monday
            | ZellerDays.Tuesday -> WeekDays.Tuesday
            | ZellerDays.Wednesday -> WeekDays.Wednesday
            | ZellerDays.Thursday -> WeekDays.Thursday
            | ZellerDays.Friday -> WeekDays.Friday
            | ZellerDays.Saturday -> WeekDays.Saturday
            | ZellerDays.Sunday -> WeekDays.Sunday
            | _ -> invalidArg "day" "invalid day"


    let toZellerDate (d: Date) =
      let zy = if d.Month < Months.March then d.Year - 1L else d.Year
      let zm = toZellerMonths d.Month |> int64
      let zd = d.Day |> int64
      (zd, zm, zy)

    //Zeller's congruence for Gregorian calendar
    //source: https://en.wikipedia.org/wiki/Zeller's_congruence
    let congruence (day, month, year) =
                    let k = year % 100L
                    let j = year / 100L
                    day + 13L * (month + 1L) / 5L + k + k /4L + j / 4L + 5L * j
                    |> (%) <| 7L
                    |> int |> enum<ZellerDays>
                    |> toWeekDays

    //Test congruence with today's date
    congruence (30L, 5L, 2016L) |> ignore

    let (|LeapYear|_|) y =
        if (y % 4L = 0L && y % 400L = 0L) || (y % 4L = 0L && y % 100L <> 0L) then
          Some y
        else None

    let isLeapYear =
        function
        | LeapYear y -> true
        | _ -> false

    //Test Leap Year
    not <| isLeapYear 2500L |> ignore

    let nbDaysInMonth year =
        function
        | Months.January  | Months.March  | Months.May
        | Months.July     | Months.August | Months.October
        | Months.December -> 31

        | Months.February  ->
            match year with
              | LeapYear y -> 29
              | _ -> 28
        | Months.April | Months.June | Months.September | Months.November -> 30

        | _ -> failwith "Invalid Month"

    let nbDaysInYear =
      function
      | LeapYear y -> 366
      | _ -> 365

    let nextDay =
        function
        | WeekDays.Monday    -> WeekDays.Tuesday
        | WeekDays.Tuesday   -> WeekDays.Wednesday
        | WeekDays.Wednesday -> WeekDays.Thursday
        | WeekDays.Thursday  -> WeekDays.Friday
        | WeekDays.Friday    -> WeekDays.Saturday
        | WeekDays.Saturday  -> WeekDays.Sunday
        | WeekDays.Sunday    -> WeekDays.Monday
        | _ -> failwith "Invalid Week Day"

    let nextMonth =
        function
        | Months.January -> Months.February
        | Months.February -> Months.March
        | Months.March -> Months.April
        | Months.April -> Months.May
        | Months.May -> Months.June
        | Months.June -> Months.July
        | Months.July -> Months.August
        | Months.August -> Months.September
        | Months.September -> Months.October
        | Months.October -> Months.November
        | Months.November -> Months.December
        | Months.December -> Months.January
        | _ -> failwith "Invalid Month"

    let nextDate date =
        if date.Day + 1 > (nbDaysInMonth date.Year date.Month) then
            let newmonth =  nextMonth date.Month
            if newmonth = Months.January then
                { Day = 1; Weekday = nextDay date.Weekday; Month = Months.January; Year = date.Year + 1L}
            else
                { Day = 1; Weekday = nextDay date.Weekday; Month = newmonth; Year = date.Year}

        else
            {date with Day = date.Day + 1; Weekday = nextDay date.Weekday}

    let alldates start endDate = Seq.unfold(fun d -> if d > endDate then None else Some (d, nextDate d)  ) start

    let findweekDay = toZellerDate >> congruence

    let readDate (input : string) =
      let [|year; month; day|] = input.Split [|' '|] |> Array.map int64
      let date = {Day = int day; Month = int month |> toMonth ; Year = year; Weekday = WeekDays.Monday }
      let weekday = findweekDay date
      {date with Weekday = weekday}


    let solution date1 date2 =
      alldates date1 date2
      |> Seq.filter(fun d -> d.Weekday = WeekDays.Sunday && d.Day = 1)
      |> Seq.length

    //[<EntryPoint>]
    let main agv =
      let t = Console.ReadLine() |> int
      Array.init t (fun _ ->
          let date1 = Console.ReadLine() |> readDate
          let date2 = Console.ReadLine() |> readDate
          solution date1 date2
      )
      |> Array.map (printfn "%d")
      |> ignore
      0

    //My Test
    let d = readDate "2016 1 1"

    // Test 1
    let d1 = readDate "1900 1 1"
    let d2 = readDate "1910 1 1"
    solution d1 d2 = 18 |> ignore

    // Test 2
    let d1' = readDate "2000 1 1"
    let d2' = readDate "2020 1 1"
    solution d1' d2' = 35 |> ignore

    //Test 3
    let d3 = readDate "1000000000000 2 2"
    let d4 = readDate "1000000001000 3 2"
    solution d3 d4 = 1720 |> ignore
