#' Box Plots
#'
#' Each element is a single plot of grouped boxplots.
#'
#' @param data
#' Dataframe
#' @param elements
#' Select the elements to plot as vector.
#' @param group
#' Select the grouping of the data set as character variable. This needs to be the
#' name of a column in the data frame.
#' @param xlegend
#' Label the x-axis.
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import magrittr
box_plot <- function(data,
                     elements,
                     group,
                     xlegend = '') {

    for (element in names(data[elements])) {
        quo_group <- ggplot2::sym(group)
        quo_element <- ggplot2::sym(element)
        sum_stat <- data %>%
            dplyr::group_by(!! quo_group) %>%
            dplyr::summarise(Number = sum(!is.na(!! quo_element)),
                             Mean = mean(!! quo_element, na.rm = TRUE),
                             Min = min(!! quo_element, na.rm = TRUE))
        print(data %>%
                  dplyr::group_by(!! quo_group) %>%
                  ggplot2::ggplot(ggplot2::aes(x = stats::reorder(!! quo_group,
                                         !! quo_element,
                                         FUN = median,
                                         na.rm = TRUE),
                             y = !! quo_element,
                             color = !! quo_group)) +
                  ggplot2::geom_boxplot() +
                  ggplot2::geom_text(data = sum_stat,
                            ggplot2::aes(x = sum_stat[[1]],
                                y = Min - 0.5*Min,
                                label = paste0('n=', Number)),
                            show.legend = FALSE) +
                  ggplot2::geom_point(data = sum_stat,
                             ggplot2::aes(x = sum_stat[[1]],
                                 y = Mean),
                             shape = 18,
                             size = 3) +
                  ggplot2::scale_y_log10() +
                  ggplot2::xlab(xlegend) +
                  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45, vjust = -1.5)))
    }
}
