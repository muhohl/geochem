#' Eigen Value Plot
#'
#' Plot the Eigen Values and their respective loadings from a PCA. Simple plot
#' that doesn't include the data points.
#'
#' @param pcobj
#' PCA object from stats::pcomp.
#' @param sel_pc
#' Select two principal components to plot.
#' @param ...
#' size of the labels and ratio of the plot
#'
#' @return
#' @export
#'
#' @examples
eigen_value_plot <- function(pcobj, sel_pc = c(1,2), ...) {

    expl_var <- (100 * pcobj$sdev[sel_pc]^2/sum(pcobj$sdev^2))
    x_var <- names(pcobj$rotation %>% tibble::as_tibble())[sel_pc[1]]
    y_var <- names(pcobj$rotation %>% tibble::as_tibble())[sel_pc[2]]

    p <- pcobj$rotation %>%
        tibble::as_tibble() %>%
        dplyr::mutate(Element = stringr::str_extract(row.names(pcobj$rotation),
                                     '[:alpha:][:alpha:]|[:alpha:]')) %>%
        dplyr::mutate(KNN = stats::kmeans(pcobj$rotation[,sel_pc], centers = 4)[[1]] %>%
                   stringr::str_extract('\\d')) %>%
        ggplot2::ggplot() +
        ggplot2::geom_segment(ggplot2::aes(x = 0,
                         y = 0,
                         xend = !! ggplot2::sym(x_var),
                         yend = !! ggplot2::sym(y_var),
                         color = KNN),
                     arrow = ggplot2::arrow(length = ggplot2::unit(0.03, "npc")),
                     size = 1) +
        ggrepel::geom_text_repel(ggplot2::aes(!! ggplot2::sym(x_var),
                                              !! ggplot2::sym(y_var),
                                     label = Element,
                                     color = KNN),
                                 size = ...) +
        ggplot2::coord_fixed(ratio = ...) +
        ggplot2::guides(color = FALSE) +
        ggplot2::xlab(paste(x_var, sprintf('(%0.1f%% explained var.)', expl_var[1]))) +
        ggplot2::ylab(paste(y_var, sprintf('(%0.1f%% explained var.)', expl_var[2])))

    return(p)
}
