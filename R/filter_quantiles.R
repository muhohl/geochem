
#' filter_quantiles
#'
#' Replaces outliers with NAs.
#'
#' @param data
#' Provide data frame in tibble format.
#' @param .cols
#' Specify numeric columns that should be screened for NAs.
#' @param probs
#' Sequence that determines step size for quantiles.
#' @param quantile_position
#' Position in the sequence determined by probs. Values within quantiles higher than
#' quantile_position will be replaced by NA
#'
#' @returns
#' data
#' @export
#' @examples

filter_quantiles <- function(
  data,
  .cols,
  probs = seq(0, 1, 0.05),
  quantile_position = 20
) {
  data_095 <- data |>
    dplyr::select(dplyr::any_of({{ .cols }})) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), \(x) {
      stats::quantile(x, na.rm = TRUE, probs = probs)[[quantile_position]]
    })) |>
    dplyr::distinct()

  data_quantiles <- tibble::tibble(ref = 1:nrow(data))

  for (element in .cols) {
    data_filtered <- data |>
      dplyr::mutate(ref = 1:dplyr::n()) |>
      dplyr::filter(
        !!dplyr::sym(element) <= data_095 |> dplyr::pull(element)
      ) |>
      dplyr::select(dplyr::all_of(c("ref", element)))
    data_quantiles <- dplyr::left_join(data_quantiles, data_filtered, by = join_by(ref))
  }

  data_quantiles <- data_quantiles |>
    dplyr::filter(dplyr::if_any(.cols = dplyr::everything(), ~ !is.na(.)))

  data_new <- dplyr::left_join(
    data |>
      dplyr::mutate(ref = 1:dplyr::n()) |>
      dplyr::select(-dplyr::any_of({{ .cols }})),
    data_quantiles,
    by = join_by(ref)
  ) |>
    dplyr::select(-ref)

  return(data_new)
}
