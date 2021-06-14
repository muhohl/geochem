#' Eigen Value plot geom for tidymodel PCA
#'
#' @param pca_rec
#' The PCA recipe, a tidymodel object.
#' @param pc_x
#' PC shown on x-axis.
#' @param pc_y
#' PC shown on y-axis.
#' @param labels
#' If TRUE labels of the arrows are plotted.
#'
#' @return ggplot object
#' @export
#'
#' @examples
geom_pca_arrows <- function(pca_rec,
                            pc_x = 1,
                            pc_y = 2,
                            labels = TRUE) {

    pca_wider <- pca_rec %>%
         recipes::tidy(id = "pca",
                       type = "coef") %>%
         tidyr::pivot_wider(names_from = component,
                            id_cols = terms)

    arrow_style <- ggplot2::arrow(length = ggplot2::unit(.1, "inches"),
                                  type = "open")

    pca_arrows <- list()
    pca_arrows <- append(pca_arrows,
                         list(
                             ggplot2::geom_segment(data = pca_wider,
                                                   ggplot2::aes(xend = !!ggplot2::sym(glue::glue("PC{pc_x}")),
                                                                yend = !!ggplot2::sym(glue::glue("PC{pc_y}"))),
                                                   x = 0,
                                                   y = 0,
                                                   color = "yellow",
                                                   size = 1.1,
                                                   arrow = arrow_style)
                         )
    )

    if (labels) {
        pca_arrows <- append(pca_arrows, list(
        ggrepel::geom_label_repel(data = pca_wider,
                                  ggplot2::aes(x = !!ggplot2::sym(glue::glue("PC{pc_x}")),
                                               y = !!ggplot2::sym(glue::glue("PC{pc_y}")),
                                               label = terms),
                                  label.size = 0.25,
                                  fill = "white",
                                  color = "#0A537D")
        ))
    }

    return(pca_arrows)

}
