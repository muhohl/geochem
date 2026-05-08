#' Impute below-detection-limit values with random numbers
#'
#' Replaces values flagged as below the detection limit (e.g. `"<0.05"`) with
#' a random number drawn uniformly between 0 and the detection limit. This is a
#' simple single-imputation approach commonly used in geochemistry to handle
#' censored data before log-transformation or multivariate analysis.
#'
#' Columns are selected by integer position via `columns`. Each selected column
#' is scanned for strings containing `split_symbol`; matching values are split
#' on that symbol, the detection limit is extracted from the right-hand part,
#' and a uniform random value in \eqn{[0, \text{limit}]} is substituted. Values
#' that do not contain `split_symbol` are left unchanged. All selected columns
#' are coerced to numeric on return.
#'
#' @param data A data frame containing at least the columns referenced by
#'   `columns`.
#' @param columns Integer vector of column positions to process.
#' @param split_symbol Character scalar used to identify and split
#'   below-detection values (default `"<"`).
#' @param digits Number of decimal places to round the imputed random value to
#'   (default `6`).
#'
#' @return The input data frame with the selected columns replaced by numeric
#'   vectors, below-detection entries substituted by random values.
#' @export
#'
#' @examples
#' d <- data.frame(Au = c("0.12", "<0.05", "0.30"), Cu = c("<1", "2.1", "0.8"))
#' random_number_imputer(d, columns = c(1, 2))
#'
#' @import magrittr
random_number_imputer <- function(data,
                                  columns,
                                  split_symbol = "<",
                                  digits = 6) {

    data_subset <- data[columns]
    split_mat <- tibble::tibble(1:nrow(data_subset),
                          nrow(data_subset):1)

    for (i in 1:ncol(data_subset)) {
        for (j in 1:nrow(data_subset)) {
            if (stringr::str_detect(data_subset[j, i], split_symbol)) {
                split_mat <- data_subset[[i]] %>%
                    stringr::str_split(split_symbol, simplify = TRUE)

                for (k in nrow(split_mat):1) {
                    if (split_mat[[k, 1]] == "" |
                        is.na(split_mat[[k, 1]])) {
                        split_mat[[k, 1]] <-
                            round(stats::runif(n = 1, min = 0,
                                        max = as.numeric(split_mat[k, 2])),
                                  digits = digits)
                    }
                }
                data_subset[[i]] <- split_mat[, 1]
            }
        }
    }

    data_subset <- data_subset %>%
        purrr::map(as.character) %>%
        purrr::map(as.numeric)
    data[columns] <- data_subset
    return(data)
}
