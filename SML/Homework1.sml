(*Date int*int*int -> year,month,day*)
fun is_older(date1 : int*int*int, date2 : int*int*int) =
    (*year 1 < year 2*)
    (#1 date1 < #1 date2)
    (*year 1 = year 2,month 1 < month2*)
    orelse (#1 date1 = #1 date2 andalso #2 date1 = #2 date2)
    (*year 1 = year 2 and ,month 1 = month2, day 1< day 2*)
    orelse (#1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 = #3 date2)

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
