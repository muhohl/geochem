#' Histograms for all selected columns
#'
#' @param data
#' @param x_axis
#' @param bins
#'
#' @return ggplot objects
#' @export
#'
#' @examples
hist_plot <- function(data, x_axis, bins){
    quo_x_axis <- dplyr::sym(x_axis)
    for (sampID in unique(data$Sample_ID)) { # potentially select column as a variable as well
        print(data %>%
                  dplyr::filter(Sample_ID == sampID) %>%
                  ggplot2::ggplot(ggplot2::aes(x = !! quo_x_axis)) +
                  ggplot2::geom_histogram(ggplot2::aes(y = ..density..), bins = bins) +
                  ggplot2::stat_function(fun = stats::dnorm,
                                args = list(mean = mean(data[[x_axis]]),
                                            sd = stats::sd(data[[x_axis]])),
                                color = "red", size = 1) +
                  ggplot2::labs(title = sampID)) +
            ggplot2::scale_x_continuous(labels = prettyNum)
    }
}
