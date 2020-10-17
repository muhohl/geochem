#' Rounds up nicely, used in the Laser Mapping function.
#'
#' @param x
#' Vetor with values that should be rounded up.
#' @param nice
#' Defines to which values x should be rounded up.
#'
#' @return
#' @export
#'
#' @examples
#'
round_up <- function(x, nice=c(seq(1, 10, 0.1))) {
    if(length(x) != 1) stop("'x' must be of length 1")
    10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}
