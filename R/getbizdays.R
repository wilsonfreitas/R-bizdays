#' Obtaining business days using other dates (or month or year) as reference
#'
#' Calculates the number of business days for some specific periof of a year
#' or a month.
#' \code{getbizdays} returns the number of business days according to a
#' reference than can be another date, a month or an year.
#'
#' @param ref a reference which represents a month or year, where the date has
#' to be found.
#' @param cal the calendar's name
#'
#' \code{getbizdays} returns the number of working days according to a reference
#' that can be a month or an year.
#' This reference can be passed as a character vector representing months
#' or years, or as a numeric vector representing years. The ISO format must be
#' used to represent years or months with character vectors.
#'
#' @examples
#' # for years
#' getbizdays(2022:2024, "Brazil/ANBIMA")
#'
#' # for months
#' getbizdays("2022-12", "Brazil/ANBIMA")
#'
#' @export
getbizdays <- function(ref, cal = bizdays.options$get("default.calendar")) {
  cal <- check_calendar(cal)
  ref <- ref(ref)

  bizdays_ <- lapply(
    seq_len(NROW(ref$ref_table)),
    function(x) count_bizdays(ref, cal, x)
  )
  unlist(bizdays_)
}

count_bizdays <- function(x, cal, ref_pos) {
  UseMethod("count_bizdays")
}

count_bizdays.by_month <- function(x, cal, ref_pos) {
  ix <- cal$dates.table[, "month"] == x$ref_table[ref_pos, "month"] &
    cal$dates.table[, "year"] == x$ref_table[ref_pos, "year"]
  sum(cal$dates.table[ix, "is_bizday"])
}

count_bizdays.by_year <- function(x, cal, ref_pos) {
  ix <- cal$dates.table[, "year"] == x$ref_table[ref_pos, "year"]
  sum(cal$dates.table[ix, "is_bizday"])
}