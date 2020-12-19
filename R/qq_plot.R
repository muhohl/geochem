#' QQ-Plots
#'
#' Creates qq-plots for all elements specified and colors the plot
#' according to group.
#'
#' @param data
#' Datatable
#' @param elements
#' List with the elements that should be plotted.
#' @param group
#' Name of the group used for coloring the plot.
#' @param color_scale
#' Specify the colorscale. Virdis is the standard color scale.
#' @param theme
#' Specify the theme. ggplot2::theme_dark() is the stadard theme.
#' @param print
#' If TRUE qq plots are printed directly if FALSE the qq-plots are
#' saved to a list, which is returned.
#'
#' @return
#' @export
#'
#' @examples
qq_plot <- function(data,
                    elements,
                    group,
                    color_scale = ggplot2::scale_color_viridis_d(),
                    theme = ggplot2::theme_dark(),
                    print = TRUE){
    # Example:
    # qq_plots_colored(data = tibble, variables = c(1:15), color = 'Samples')
    quo_color <- ggplot2::sym(group)
    plot_list <- setNames(as.list(seq_along(elements)), elements)

    for (element in elements) {
        quo_element <- ggplot2::sym(element)

        data_arranged <- data %>%
            dplyr::arrange(!! quo_element)

        data_arranged$t <- stats::quantile(stats::rnorm(1000),
                                    seq(0, 100,
                                        length.out =
                                            nrow(data))/100)

        p <- ggplot2::ggplot(data = data_arranged,
                             ggplot2::aes(x=t, y= !! quo_element)) +
            ggplot2::geom_qq_line(mapping = ggplot2::aes(sample = !! quo_element),
                         size = 1.2) +
            ggplot2::geom_point(ggplot2::aes(colour = !! quo_color),
                       size = 3) +
                color_scale +
                theme

        if (is.numeric(data[[group]])) {
            p <- p + scale_color_viridis_c() + theme_dark()
        }

        if (print) {
            print(p)
        } else {
           plot_list[[element]] <- p
        }
    }
    if (!print) return(plot_list)
}
