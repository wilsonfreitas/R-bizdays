#' Load builtin calendars
#'
#' bizdays comes with builtins calendars:
#'
#' - `actual`: every day is a business day.
#' - `weekends`: Saturdays and Sundays are not business days; no holidays.
#' - `Brazil/ANBIMA`: Brazilian national banking holidays, from 2001 to 2079.
#' - `Brazil/BMF`: the same holidays as `Brazil/ANBIMA`, but it starts in 1990.
#' - `Brazil/B3`: days without a trading session at the B3 exchange, from
#'   1990 to 2079.
#'
#' This function creates and registers these calendars.
#' Once the calendars are loaded they can be used directly by their names.
#'
#' This function is called in package `.onAttach`, so it is not necessary to
#' call it directly.
#' It is for internal use, package development or in situations where the
#' user wants to call bizdays functions without attach the package.
#'
#' @section Differences between `Brazil/B3` and `Brazil/BMF`:
#' The two calendars have the same weekdays and settings, and every holiday of
#' `Brazil/BMF` is also a holiday of `Brazil/B3`. They are equal from 1990 to
#' 2000. Since 2001 `Brazil/B3` has these extra days without a trading session:
#'
#' - December 24th and the last weekday of the year (December 31st, or the
#'   Friday before it when December 31st falls on a weekend). Banks are open
#'   for internal work on these days, so they are business days in
#'   `Brazil/BMF` and `Brazil/ANBIMA`.
#' - The São Paulo holidays January 25th and July 9th, from 2001 to 2021
#'   (except 2020-07-09, when B3 was open). Since 2022 B3 trades on these days.
#' - November 20th (Black Consciousness Day) from 2006 to 2019, when it was a
#'   São Paulo city holiday only (it was not a closing day in 2020). Since 2024 it is a national holiday and it
#'   is in both calendars.
#' - 2014-06-12, the opening day of the World Cup in São Paulo.
#'
#' Up to 2027 the `Brazil/B3` holidays come from the calendars published by
#' B3. After 2027 they are estimated by a rule: the `Brazil/ANBIMA` holidays
#' plus December 24th and the last weekday of the year.
#'
#' @md
#' @return Has no return
#'
#' @examples
#' bizdays::load_builtin_calendars()
#' bizdays::calendars()
#' bizdays::is.bizday("2020-01-01", "Brazil/ANBIMA")
#' @export
load_builtin_calendars <- function() {
  cal_dir <- system.file("extdata", package = "bizdays")
  fnames <- list.files(cal_dir, pattern = "json$", full.names = TRUE)
  lapply(fnames, load_calendar)
  bizdays.options$set(default.calendar = "actual")
}