#' QQ plot with optional group colour mapping
#'
#' Plots a quantile-quantile (QQ) diagram for a single element, optionally
#' colouring points by a grouping variable. Because [ggplot2::geom_qq()] does
#' not support per-point colour mapping, theoretical normal quantiles are
#' computed via [stats::ppoints()] (identical to `geom_qq`'s internal method),
#' the element values are sorted to match, and points are drawn with
#' [ggplot2::geom_point()] so that any colour aesthetic works freely.
#'
#' To produce plots for multiple elements use `lapply` or `purrr::map`:
#' ```r
#' lapply(c("Au_ppm", "Cu_ppm"), \(el) ggcoloredqq(data, el, group = "Lith"))
#' ```
#'
#' @param data A data frame.
#' @param element Character scalar; name of the column to plot on the y-axis.
#' @param group Character scalar; column name used for point colour or fill
#'   mapping. If `NULL` (default) points are drawn without a group aesthetic.
#' @param fill Logical; if `TRUE` `group` is mapped to `fill` (requires a
#'   filled point shape such as `shape = 21`). If `FALSE` (default) `group` is
#'   mapped to `colour`.
#' @param qq_line Logical; if `TRUE` (default) the theoretical normal reference
#'   line is drawn via [ggplot2::geom_qq_line()]. Set to `FALSE` to omit it.
#' @param linewidth Line width of the QQ reference line (default `1`).
#' @param size Point size (default `1`).
#' @param color_scale A ggplot2 colour or fill scale applied when `group` is a
#'   character or factor. Numeric `group` columns automatically switch to the
#'   continuous viridis variant. Defaults to [ggplot2::scale_color_viridis_d()]
#'   when `fill = FALSE` and [ggplot2::scale_fill_viridis_d()] when
#'   `fill = TRUE`.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' ggcoloredqq(geochemdata, "Au_ppm", group = "Lithology")
#' ggcoloredqq(geochemdata, "Au_ppm")
#' }
ggcoloredqq <- function(data,
                    element,
                    group       = NULL,
                    fill        = FALSE,
                    qq_line     = TRUE,
                    linewidth   = 1,
                    size        = 1,
                    color_scale = if (fill) ggplot2::scale_fill_viridis_d()
                                  else      ggplot2::scale_color_viridis_d()) {

    quo_element <- ggplot2::sym(element)
    theoretical <- stats::qnorm(stats::ppoints(nrow(data)))

    data_arranged <- data |>
        dplyr::arrange(!!quo_element) |>
        dplyr::mutate(.theoretical = theoretical)

    p <- ggplot2::ggplot(
            data_arranged,
            ggplot2::aes(x = .theoretical, y = !!quo_element)
        ) +
        ggplot2::labs(x = "Theoretical quantiles", y = element)

    if (qq_line) {
        p <- p + ggplot2::geom_qq_line(
            ggplot2::aes(sample = !!quo_element),
            linewidth = linewidth
        )
    }

    if (!is.null(group)) {
        quo_group <- ggplot2::sym(group)
        is_continuous <- is.numeric(data[[group]])

        if (fill) {
            p <- p +
                ggplot2::geom_point(
                    ggplot2::aes(fill = !!quo_group),
                    size = size, shape = 21
                ) +
                if (is_continuous) ggplot2::scale_fill_viridis_c() else color_scale
        } else {
            p <- p +
                ggplot2::geom_point(
                    ggplot2::aes(colour = !!quo_group),
                    size = size
                ) +
                if (is_continuous) ggplot2::scale_color_viridis_c() else color_scale
        }
    } else {
        p <- p + ggplot2::geom_point(size = size)
    }

    p
}
