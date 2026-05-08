#' Replace outliers with NA by quantile threshold
#'
#' Replaces values in the selected columns that fall above (or below) a
#' quantile threshold with `NA`. All other columns are left unchanged.
#'
#' @param data A data frame.
#' @param .cols Column names or positions passed to [dplyr::any_of()].
#' @param probs Numeric scalar; the quantile probability used as the threshold
#'   (default `0.95`).
#' @param upper_or_lower `"upper"` (default) to replace values **above** the
#'   threshold with `NA`, or `"lower"` to replace values **below** it.
#'
#' @return The input data frame with out-of-range values in `.cols` set to `NA`.
#' @export
#'
#' @examples
#' d <- data.frame(x = c(1, 2, 3, 4, 100), y = c(5, 6, 7, 8, 9))
#' filter_quantiles(d, .cols = c("x", "y"), probs = 0.95)
filter_quantiles <- function(data,
                             .cols,
                             probs          = 0.95,
                             upper_or_lower = "upper") {
    if (!upper_or_lower %in% c("upper", "lower")) {
        stop("upper_or_lower must be either 'upper' or 'lower'.")
    }
    data |>
        dplyr::mutate(dplyr::across(
            dplyr::any_of({{ .cols }}),
            \(x) {
                threshold <- stats::quantile(x, probs = probs, na.rm = TRUE)
                if (upper_or_lower == "upper") dplyr::if_else(x <= threshold, x, NA)
                else                           dplyr::if_else(x >= threshold, x, NA)
            }
        ))
}
