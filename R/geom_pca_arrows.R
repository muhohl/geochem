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
#' @param labels_fill
#' Fill color of the labels
#' @param labels_color
#' Color of the labels text
#' @param labels_size
#' Size of the labels
#' @param labels_font_size
#' Size of the font in the labels
#' @param labels_rect_padding
#' Size of the padding around the labels
#' @param ...
#' Arguments passed on to geom_label_repel, which is drawing the arrows.
#'
#' @return ggplot object
#' @export
#'
#' @examples
geom_pca_arrows <- function(pca_rec,
                            pc_x = 1,
                            pc_y = 2,
                            labels = TRUE,
                            labels_fill = "white",
                            labels_color = "#0A537D",
                            labels_size = 0.25,
<<<<<<< HEAD
                            label_font_size = 3,
                            label_rect_padding = 0.25,
=======
                            labels_font_size = 3,
                            labels_rect_padding = 0.25,
>>>>>>> b360b8f636e46b9e10cfab76e7231e233914ba05
                            ...) {

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
                                                   arrow = arrow_style,
                                                   ...)
                         )
    )

    if (labels) {
        pca_arrows <- append(pca_arrows, list(
            ggrepel::geom_label_repel(data = pca_wider,
                                      ggplot2::aes(x = !!ggplot2::sym(glue::glue("PC{pc_x}")),
                                                   y = !!ggplot2::sym(glue::glue("PC{pc_y}")),
                                                   label = terms),
                                      label.size = labels_size,
<<<<<<< HEAD
                                      size = label_font_size,
                                      label.padding = label_rect_padding,
=======
                                      size = labels_font_size,
                                      label.padding = labels_rect_padding,
>>>>>>> b360b8f636e46b9e10cfab76e7231e233914ba05
                                      fill = labels_fill,
                                      color = labels_color)
        ))
    }

    return(pca_arrows)

}
