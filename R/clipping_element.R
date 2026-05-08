#' Plotly laser map for selecting a clipping threshold
#'
#' Renders an interactive plotly raster map of one element from a laser
#' ablation dataset. Use the map to visually determine a cut-off value;
#' the function doesn't do the actual clipping — apply [dplyr::filter()]
#' afterwards.
#'
#' Column names `X` and `Y` are normalised to lowercase automatically.
#'
#' @param elmnt Character scalar; name of the element column to display.
#' @param data Data frame containing `x`/`y` coordinate columns and at least
#'   the element column named by `elmnt`.
#'
#' @return A `plotly` htmlwidget.
#' @export
#'
#' @examples
#' \dontrun{
#' clipping_element("Au_ppm", laser_data)
#' }
clipping_element <- function(elmnt, data) {
    data <- dplyr::rename_with(data, tolower, dplyr::any_of(c("X", "Y")))

    p <- data |>
        ggplot2::ggplot(ggplot2::aes(
            x = x,
            y = y,
            fill = !!ggplot2::sym(elmnt),
            text = paste0(elmnt, ": ", round(!!ggplot2::sym(elmnt), digits = 1))
        )) +
        ggplot2::geom_raster(interpolate = TRUE) +
        ggplot2::scale_fill_viridis_c(labels = scales::label_number()) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::ggtitle(paste(elmnt, "(ppm)")) +
        ggplot2::labs(fill = "", x = "", y = "") +
        ggplot2::theme_bw() +
        ggplot2::theme(
            panel.border = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            axis.text = ggplot2::element_blank(),
            axis.title = ggplot2::element_blank()
        )

    plotly::ggplotly(p, tooltip = "text")
}
