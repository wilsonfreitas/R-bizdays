#' @title Set Operations
#' replace as.vector invoke with as.Date
#' @rdname set
#' @export
setdiff.Date <- function(x, y) {
  x <- as.Date(x)
  y <- as.Date(y)
  unique(if (length(x) || length(y))
    x[match(x, y, 0L) == 0L]
    else x)
}

#' @rdname set
#' @export
setequal.Date <- function(x, y) {
  x <- as.Date(x)
  y <- as.Date(y)
  all(c(match(x, y, 0L) > 0L, match(y, x, 0L) > 0L))
}

#' @rdname set
#' @export
union.Date <- function(x, y) {
  unique(c(as.Date(x), as.Date(y)))
}

#' @rdname set
#' @export
intersect.Date <- function(x, y) {
  y <- as.Date(y)
  unique(y[match(as.Date(x), y, 0L)])
}