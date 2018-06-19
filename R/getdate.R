
MONTH <- function(x) as.integer(format(x, "%m"))

YEAR <- function(x) as.integer(format(x, "%Y"))

#' @export
ref <- function(x, ...) UseMethod("ref")

#' @export
ref.Date <- function(x, ym = c("month", "year"), ...) {
  ym = match.arg(ym)
  that <- if (ym == "month") {
    list(
      dates = x,
      by_month = TRUE,
      year_month = cbind(year = YEAR(x), month = MONTH(x))
    )
  } else {
    list(
      dates = x,
      by_month = FALSE,
      year_month = cbind(year = YEAR(x))
    )
  }
  structure(that, class = "ref")
}

#' @export
ref.character <- function(x, ...) {
  that <- if (all(grepl("^(\\d{4})-(\\d{2})$", x))) {
    mx <- regmatches(x, regexec("^(\\d{4})-(\\d{2})$", x))
    mx <- do.call(rbind, mx)
    list(
      by_month = TRUE,
      year_month = cbind(year = as.integer(mx[,2]), month = as.integer(mx[,3]))
    )
  } else if (all(grepl("^(\\d{4})$", x))) {
    mx <- regmatches(x, regexec("^(\\d{4})$", x))
    mx <- do.call(rbind, mx)
    list(
      by_month = FALSE,
      year_month = cbind(year = as.integer(mx[,2]))
    )
  } else if (all(grepl("^\\d{4}-\\d{2}-\\d{2}$", x))) {
    do.call(ref.Date, append(list(...), list(x = as.Date(x))))
  } else
    stop("Invalid character ref ", x)
  structure(that, class = "ref")
}

#' @export
ref.numeric <- function(x, ...) {
  that <- list(
    by_month = FALSE,
    year_month = cbind(year = x)
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

getnthweekday_ <- function(pos, ref, cal, wday, ref_pos = 1) {
  ix <- if (ref$by_month) {
    ix_ <- cal$dates.table[,"month"] == ref$year_month[ref_pos, "month"] &
      cal$dates.table[,"year"] == ref$year_month[ref_pos, "year"]
    ix_ & cal$dates.table[,"weekday"] == wday
  } else {
    ix_ <- cal$dates.table[,"year"] == ref$year_month[ref_pos, "year"]
    ix_ & cal$dates.table[,"weekday"] == wday
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
  } else if (tok[2] %in% WEEKDAYS) {
    wday <- which(tok[2] == WEEKDAYS)
    date_res <- lapply(seq_len(NROW(ref$year_month)),
                       function(x) getnthweekday_(n, ref, cal, wday, x))
    as.Date(unlist(date_res), origin = as.Date("1970-01-01"))
  } else
    stop("Invalid expr", expr)
}

WEEKDAYS <- c("thu", "fri", "sat", "sun", "mon", "tue", "wed")
