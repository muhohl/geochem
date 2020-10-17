#' Helper function for the Laser Mapping function. Checks the potential legend
#' breaks if some breaks occur twice. This might happen since values are rounded
#' to look nicer.
#'
#' @param breaks_fun
#' The values for breaks defined stored in a list defined by the Laser Mapping
#' function.
#' @param small_scale
#' Boolean value that decides if a small scale is necassary.
#'
#' @return
#' @export
#'
#' @examples
double_values_checker <- function(breaks_fun = small_breaks_list[[1]],
                                  small_scale = small_breaks_list[[2]]) {

    if (length(unique(breaks_fun)) != length(breaks_fun)) {
        small_scale <- TRUE
    }

    if (!small_scale) {
        for (j in 2:length(breaks_fun)) {
            if (breaks_fun[j] <= breaks_fun[j-1]) {
                small_scale <- TRUE
            }
        }
    }
    return(small_scale)
}
