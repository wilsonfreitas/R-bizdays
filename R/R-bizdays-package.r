#' Tools for business days calculations
#' 
#' In many countries the standard approach to price derivatives and fixed income
#' instruments involves the use of business days. In Brazil, for example, the
#' great majority of financial insturments are priced on business days counting
#' rules. Given that the use of business days is somehow vital to handle many
#' tasks. That's the reason why bizdays came up, to make these tasks easier.
#' Excel's NETWORKDAYS is fairly at hand and once you have a list of
#' holidays this is quite easy to put your data into a spreadsheet and make
#' things happen.
#' \code{bizdays} brings that ease to R.
#' 
#' Although R's users have similar feature in packages like \code{RQuantLib} and
#' \code{timeDate} it doesn't come for free. Users have to do some stackoverflow and
#' google in order to get this task accomplished. \code{bizdays} tries to fit your
#' needs. It is a tiny package which is dramactically focused on that simple
#' task: calculations involving business days for a given list of holidays. It
#' doesn't implement specific holidays guessing algorithms.
#'
#' @name bizdays-package
#' @docType package
NULL
