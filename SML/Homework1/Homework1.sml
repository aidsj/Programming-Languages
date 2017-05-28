(*Date int*int*int -> year,month,day*)
fun is_older(date1 : int*int*int, date2 : int*int*int) =
    (*year 1 < year 2*)
    (#1 date1 < #1 date2)
    (*year 1 = year 2,month 1 < month2*)
    orelse (#1 date1 = #1 date2 andalso #2 date1 < #2 date2)
    (*year 1 = year 2 and ,month 1 = month2, day 1< day 2*)
    orelse (#1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2)

fun number_in_month(dates: (int*int*int) list, month: int) =
    if null dates
    then 0
    else
      let val rest_dates = number_in_month(tl dates,month) (*record rest of dates*)
      in
        if #2 (hd dates) = month
        then 1+rest_dates
        else rest_dates
      end

fun number_in_months(dates: (int*int*int) list, months: int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates: (int*int*int) list, month: int) =
    if null dates
    then []
    else
      let val rest_dates = dates_in_month(tl dates,month) (*record rest of dates*)
      in
        if #2 (hd dates) = month
        then hd dates :: rest_dates
        else rest_dates
      end

fun dates_in_months(dates: (int*int*int) list, months: int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(str: string list, n : int) =
    if n = 1
    then hd str
    else get_nth(tl str,n - 1)

(*Take a date return string January 20, 2013 format*)
fun date_to_string(date : (int*int*int)) =
    let val month = ["January", "February", "March", "April", "May", "June",
                        "July", "August", "September", "October", "November", "December"]
    in
        get_nth(month, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum(sum: int, values: int list) =
    if sum <= hd values
    then 0
    else 1 + number_before_reaching_sum(sum - (hd values),tl values)

fun what_month(day :int) =
    let
        val days = [31,28,31,30,31,30,31,31,30,31,30,31]
        (*val nth = number_before_reaching_sum(day, days)*)
        (*val months = ["January", "February", "March", "April", "May", "June",
                            "July", "August", "September", "October", "November", "December"] *)
    in
        number_before_reaching_sum(day,days) + 1
    end

fun month_range(day1 : int, day2: int) =
    if day1 > day2
    then []
    else [what_month(day1)] @ month_range(day1+1,day2)


fun oldest(dates : (int*int*int) list) =
    if null dates
    then NONE
    else
        let
            val oldest_tl = oldest(tl dates)
        in
            if isSome oldest_tl andalso is_older(valOf oldest_tl,hd dates)
            then oldest_tl
            else SOME (hd dates)
        end
(*Check if the tail of list have the same element as head of list if it does remove it from tail*)
fun rm_duplicates(nums : int list) =
    if null nums
    then []
    else let fun rm_dup(head: int, tail: int list) =
            if null tail then []
            else let val tail_filtered = rm_dup(head, tl tail)
                 in
                    if  head = hd tail  then tail_filtered
                    else hd tail :: tail_filtered
                end
        in
          hd nums :: rm_duplicates(rm_dup(hd nums,tl nums))
        end

fun number_in_months_challenge (dates: (int * int * int) list, months: int list) =
    number_in_months(dates, rm_duplicates(months))

fun dates_in_months_challenge (dates: (int * int * int) list, months: int list) =
    dates_in_months(dates, rm_duplicates(months))

fun reasonable_date (date : int * int * int) =
    let
        val days = [31,28,31,30,31,30,31,31,30,31,30,31]
        val leap_days = [31,29,31,30,31,30,31,31,30,31,30,31]
        fun is_leap_year(year: int) =
            (year mod 400 = 0) orelse (year mod 100 <> 0 andalso year mod 4 = 0)
        fun get_nth(days: int list, n : int) =
            if n = 1
            then hd days
            else get_nth(tl days,n - 1)
    in
      (#1 date >= 1) andalso (#2 date >= 1) andalso (#2 date <= 12) andalso (#3 date >= 1) andalso
      (if is_leap_year(#1 date) then (get_nth(leap_days,#2 date) >= #3 date) else (get_nth(days,#2 date) >= #3 date))
    end
