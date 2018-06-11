
# ----

cleanup <- function(x) {
  x <- Filter(length, x)
  Filter(Negate(is.null), x)
}

handle_adjust <- function(x) {
  switch (x,
          adjust.next = "following",
          adjust.previous = "preceding",
          adjust.none = NULL
  )
}

cal2list <- function(cal) {
  
  cal_list <- list(
    name = cal$name,
    weekdays = cal$weekdays,
    holidays = cal$holidays,
    financial = cal$financial,
    adjust.from = handle_adjust(cal$adjust.from_label),
    adjust.to = handle_adjust(cal$adjust.to_label)
  )
  
  cleanup(cal_list)
}

# Brazil/ANBIMA ----

cal_name <- "Brazil/ANBIMA"
cal <- calendars()[[cal_name]]
cal_list <- cal2list(cal)
fname <- file.path("inst/extdata", paste0(gsub("/", "_", cal_name), ".json"))
writeLines(jsonlite::toJSON(cal_list, auto_unbox = TRUE, pretty = TRUE), fname)

# actual ----

cal_name <- "actual"
cal <- calendars()[[cal_name]]
cal_list <- cal2list(cal)
fname <- file.path("inst/extdata", paste0(gsub("/", "_", cal_name), ".json"))
writeLines(jsonlite::toJSON(cal_list, auto_unbox = TRUE, pretty = TRUE), fname)

# weekends ----

cal_name <- "weekends"
cal <- calendars()[[cal_name]]
cal_list <- cal2list(cal)
fname <- file.path("inst/extdata", paste0(gsub("/", "_", cal_name), ".json"))
writeLines(jsonlite::toJSON(cal_list, auto_unbox = TRUE, pretty = TRUE), fname)
