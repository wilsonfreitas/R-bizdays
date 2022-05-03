#' Load builtin calendars
#'
#' bizdays comes with builtins calendars:
#'
#' - actual
#' - weekends
#' - Brazil/ANBIMA
#' - Brazil/B3
#'
#' This function creates and registers these calendars.
#' Once the calendars are loaded they can be used directly by their names.
#'
#' This function is called in package `.onAttach`, so it is not necessary to
#' call it directly.
#' It is for internal use, package development or in situations where the
#' user wants to call bizdays functions without attach the package.
#'
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