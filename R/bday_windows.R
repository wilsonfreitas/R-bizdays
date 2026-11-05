# Function will fit business day 'windows' around 'Edate', according to prespecified lengths
# function has option to fit an additional businessday 'window' prior to first window, of a prespecified length
# results:
  # bdays == {n:k} & length(bdays) == n+k+1
      # where
      # bdays is a sequence of business days fitted around a specified date, 
        # so that there are a specified amount of days PRIOR and AFTER the specified day
      # n is the amount of business days PRIOR to specified day
      # k is the amount of business days AFTER to specified day
  # est_bdays == {(m-250):m} & length(est_bdays) == est_len
      # where
      # est_bdays is a sequence of business days that is equal to the length specified. Does not overlap with 'bdays'
      # m is the last business date of a window ending immediately PRIOR to bdays's window (does not overlap) 
b_days <- function(Edate, wind_len, est_len, cal) {
  Edate <- as.Date(Edate)
  adj1 <- 1
  
  # event window
  lower <- bizdays::add.bizdays(Edate, wind_len[[1]], cal)
  upper <- bizdays::add.bizdays(Edate, wind_len[[2]], cal)
  bdays <- bizdays::bizseq(as.Date(lower), as.Date(upper), cal)
  
  # estimation widow
  est_end <- bizdays::add.bizdays(Edate, (wind_len[[1]]-adj1), cal)
  if (est_end >= lower) {
    while (est_end >= lower) {
      adj <- adj + 1
      est_end <-
        bizdays::add.bizdays(Edate, (wind_len[[1]] - adj1), cal)
    }
  }
  adj2 <- 1
  adj3 <- 1
  est_start <- bizdays::add.bizdays(Edate, (wind_len[[1]]-adj2-est_len), cal)
  est_bdays <- bizdays::bizseq(est_start, est_end, cal)
  
  # Logic ensures estimation days match requested estimation length
  est_start2 <- bizdays::add.bizdays(Edate,
                                     (wind_len[[1]] - adj2 - est_len),
                                     cal)
  est_start3 <- bizdays::add.bizdays(Edate,
                                     (wind_len[[1]] - adj3 - est_len),
                                     cal)
  est_bdays2 <- length(bizdays::bizseq(est_start2, est_end, cal))
  est_bdays3 <- length(bizdays::bizseq(est_start3, est_end, cal))
  if (length(est_bdays) != est_len) {
    while (all((est_bdays2 != est_len), (est_bdays3 != est_len))) {
      adj2 <- adj2 + 1
      adj3 <- adj3 - 1
      
      est_start2 <- bizdays::add.bizdays(Edate,
                                         (wind_len[[1]] - adj2 - est_len),
                                         cal)
      est_start3 <- bizdays::add.bizdays(Edate,
                                         (wind_len[[1]] - adj3 - est_len),
                                         cal)
      
      est_bdays2 <-
        length(bizdays::bizseq(est_start2, est_end, cal))
      est_bdays3 <-
        length(bizdays::bizseq(est_start3, est_end, cal))
    }
    if (est_bdays2 == est_len) {
      est_bdays <- bizdays::bizseq(est_start2, est_end, cal)
    } else if (est_bdays3 == est_len) {
      est_bdays <- bizdays::bizseq(est_start3, est_end, cal)
    }
  }
  # final check
  overlap <- as.Date(
    intersect(bdays,
              est_bdays),
    origin="1970-01-01")
  if (length(overlap)!=0) {
    message("There is an unexpected overlap between event and estimation windows. \n Please debug function logic.")
  }
  # package results and export
  bundle <- setNames(list(bdays, est_bdays),
                     c("event_window", "estimation_window"))
  return(bundle)
}

# Will only fit a sequence of business days fitted around a specified date, 
# so that there are a specified amount of days PRIOR and AFTER the specified day
# (like the first object returned above)
b_days_around_date <- function(e_day, wind_len, cal) {
  e_day <- as.Date(e_day)
  
  lower <- bizdays::add.bizdays(e_day, wind_len[[1]], cal)
  upper <- bizdays::add.bizdays(e_day, wind_len[[2]], cal)
  
  bdays <- bizdays::bizseq(as.Date(lower), as.Date(upper), cal)
  
  return(bdays)
}