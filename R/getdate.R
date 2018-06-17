
MONTH <- function(x) as.integer(format(x, "%m"))

YEAR <- function(x) as.integer(format(x, "%Y"))

#' @export
ref <- function(x, ...) UseMethod("ref")

#' @export
ref.Date <- function(x, ym = c("month", "year"), ...) {
  ym = match.arg(ym)
  that <- list(
    dates = x,
    by_month = (ym == "month"),
    year_month = cbind(year = YEAR(x), month = MONTH(x))
  )
  structure(that, class = "ref")
}

nth2int <- function(x) {
  rx <- regexec("^(\\d+)th$", x)
  mx <- regmatches(x, rx)[[1]]
  if (length(mx) == 0)
    stop("Invalid position nth", x)
  as.integer(mx[[2]])
}

getnth_ <- function(x) {
  switch(
    x,
    first = 1,
    second = 2,
    third = 3,
    last = -1,
    `1st` = 1,
    `2nd` = 2,
    `3rd` = 3,
    nth2int(x)
  )
}

getnthday_ <- function(pos, ref, cal, ref_pos = 1, use_bizday = FALSE) {
  ix <- if (ref$by_month) {
    ix_ <- cal$dates.table[,"month"] == ref$year_month[ref_pos, "month"] &
      cal$dates.table[,"year"] == ref$year_month[ref_pos, "year"]
    if (use_bizday) ix_ & cal$dates.table[,"is_bizday"] == 1 else ix_
  } else {
    ix_ <- cal$dates.table[,"year"] == ref$year_month[ref_pos, "year"]
    if (use_bizday) ix_ & cal$dates.table[,"is_bizday"] == 1 else ix_
  }
  x <- cal$dates.table[ix,]
  pos <- if (pos < 0) NROW(x) else pos
  res <- as.Date(x[pos,"dates"], origin = as.Date("1970-01-01"))
  unname(res)
}

#' @export
getdate <- function(expr, ref, cal = bizdays.options$get("default.calendar")) {
  cal <- check_calendar(cal)
  tok <- strsplit(expr, "\\s+")[[1]]
  if (length(tok) != 2)
    stop("Invalid expr", expr)
  n = getnth_(tok[1])
  if (tok[2] == "day") {
    date_res <- lapply(seq_len(NROW(ref$year_month)),
           function(x) getnthday_(n, ref, cal, x))
    as.Date(unlist(date_res), origin = as.Date("1970-01-01"))
  } else if (tok[2] == "bizday") {
    date_res <- lapply(seq_len(NROW(ref$year_month)),
                       function(x) getnthday_(n, ref, cal, x, TRUE))
    as.Date(unlist(date_res), origin = as.Date("1970-01-01"))
  }
  #   getnthbizday_(n, year, month, cal)
  # else if (tok[2] %in% self.WEEKDAYS)
  #   getnthweekday_(n, tok[2], year, month, cal)
  # else
  #   stop("Invalid day", tok[2])
  # as.Date("2018-01-01")
}