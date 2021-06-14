#' Title
#'
#' @param pca_rec
#' The PCA recipe, a tidymodel object.
#' @param pc_x
#' PC shown on x-axis.
#' @param pc_y
#' PC shown on y-axis.
#'
#' @return ggplot object
#' @export
#'
#' @examples
pca_plot <- function(pca_rec,
                  pc_x = 1,
                  pc_y = 2,
                  ...) {

       pca_all_var <- pca_rec %>%
        recipes::tidy(id = "pca",
                      type = "variance") %>%
        dplyr::filter(terms == "percent variance") %>%
        pull(value)

    base_plot <- recipes::bake(pca_rec,
                               new_data = NULL) %>%
        ggplot2::ggplot(ggplot2::aes(!!ggplot2::sym(glue::glue("PC{pc_x}")),
                                     !!sym(glue::glue("PC{pc_y}")))) +
        ggplot2::labs(x = glue::glue("PC {pc_x} [{round(pca_all_var[pc_x], 2)}%]"),
                      y = glue::glue("PC {pc_y} [{round(pca_all_var[pc_y], 2)}%]"),
                      caption = glue::glue("*n =* {pull(count(recipes::bake(pca_rec, new_data = NULL)))}"),
                      ...) +
        ggplot2::theme(axis.title.x = ggtext::element_markdown(),
                       axis.title.y = ggtext::element_markdown(),
                       plot.caption = ggtext::element_markdown(),
                       plot.title.position = "plot",
                       plot.caption.position = "plot",
                       legend.position = "top",
                       plot.margin = ggplot2::margin(0,25,10,15)) +
        ggplot2::coord_fixed()

    return(base_plot)
}
