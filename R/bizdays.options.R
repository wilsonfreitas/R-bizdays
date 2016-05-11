# merge elements of y into x with the same names
merge_list = function(x, y) {
  x[names(y)] = y
  x
}

# new_defaults - creates a settings object
new_defaults <- function(value=list()) {
  defaults <- value
  
  get <- function(name, default=FALSE, drop=TRUE) {
    if (default)
      defaults <- value  # this is only a local version
    if (missing(name))
      defaults
    else {
      if (drop && length(name) == 1)
        defaults[[name]]
      else
        defaults[name]
    }
  }
  set <- function(...) {
    dots <- list(...)
    if (length(dots) == 0) return()
    if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]]))
      if (length(dots <- dots[[1]]) == 0) return()
    defaults <<- merge(dots)
    invisible(NULL)
  }
  merge <- function(values) merge_list(defaults, values)
  restore <- function(target = value) defaults <<- target
  
  list(get=get, set=set, merge=merge, restore=restore)
}

#' bizdays' options
#' 
#' \code{bizdays.options} defines option parameters used internally in \code{bizdays}.
#' 
#' @format
#' A \code{list} object with \emph{methods} \code{get} and \code{set} attached to.
#' 
#' @details
#' Parameters are stored in \code{bizdays.options} using \code{get} and \code{set}
#' 
#' \preformatted{
#' bizdays.options$set(option.key=value)
#' bizdays.options$get("option.key")
#' }
#' 
#' \code{bizdays} supports the following parameter: 
#' 
#' \itemize{
#' \item{\code{default.calendar}: }{the default calendar to be used with the
#' functions: \code{bizdays}, \code{bizdayse}, \code{adjust.next},
#' \code{adjust.previous}, \code{is.bizday}, \code{bizseq}, \code{offset}.}
#' }
#' 
#' @examples
#' create.calendar(name="actual")
#' bizdays.options$set(default.calendar="actual")
#' bizdays("2013-07-12", "2013-07-22")
#' 
#' @export
bizdays.options <- new_defaults()

