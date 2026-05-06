#' Spider (multi-element) plot
#'
#' Produces faceted spider diagrams — one panel per group — with median lines
#' and optional interquartile ribbons. Each panel highlights its own group while
#' showing all other groups in the background.
#'
#' @param data A data frame containing a grouping column and numeric element
#'   columns.
#' @param elements Character vector of column names to include. The first
#'   element is used as the grouping variable; the remainder are plotted on the
#'   x-axis.
#' @param group Name of the grouping column (character scalar).
#' @param levels Character vector giving the desired x-axis order of elements.
#'   Defaults to `elements`.
#' @param ribbon If `TRUE` (default), the interquartile range is drawn as a
#'   shaded ribbon. If `FALSE`, individual lines are plotted instead.
#' @param ncol Number of columns in the facet grid.
#' @param ... Additional arguments passed to [ggplot2::theme()].
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' ggspider(
#'   data = whole_rock_data,
#'   elements = c("rock_type", "La", "Ce", "Nd", "Sm", "Eu", "Gd", "Yb", "Lu"),
#'   group = "rock_type"
#' )
#'
#' @import magrittr
ggspider <- function(
    data,
    elements,
    group,
    levels = elements,
    ribbon = TRUE,
    ncol = NULL,
    ...
) {
    By_Group <- function(data, group, elements, levels) {
        quo_group <- ggplot2::sym(group)
        data <- data %>% dplyr::select(elements)
        ncols <- ncol(data)
        data %>%
            tidyr::pivot_longer(
                2:ncols,
                names_to = "elements",
                values_to = "ppm"
            ) %>%
            dplyr::group_by(!!quo_group, elements) %>%
            dplyr::summarise(Median = stats::median(ppm, na.rm = TRUE)) %>%
            dplyr::mutate(elements = factor(elements, levels = levels))
    }

    By_Gather <- function(data, elements, levels) {
        data <- data %>% dplyr::select(elements)
        ncols <- ncol(data)
        data %>%
            dplyr::mutate(ID = 1:dplyr::n()) %>%
            tidyr::pivot_longer(
                2:ncols,
                names_to = "elements",
                values_to = "ppm"
            ) %>%
            dplyr::mutate(elements = factor(elements, levels = levels))
    }

    By_Gather_Ribbon <- function(data, group, elements, levels) {
        quo_group <- dplyr::sym(group)
        By_Gather(data = data, elements = elements, levels = levels) %>%
            dplyr::group_by(elements, !!quo_group) %>%
            dplyr::summarise(
                min = stats::quantile(ppm, na.rm = TRUE)[[2]],
                max = stats::quantile(ppm, na.rm = TRUE)[[4]],
                median = stats::median(ppm, na.rm = TRUE)
            ) %>%
            dplyr::mutate(ID = 1:dplyr::n())
    }

    quo_group <- ggplot2::sym(group)

    data1 <- By_Group(
        data = data,
        group = group,
        elements = elements,
        levels = levels
    )

    data2 <- data1
    data2 <- dplyr::rename(data2, "XXX" = 1)

    if (ribbon == FALSE) {
        spider_p <- By_Gather(
            data = data,
            elements = elements,
            levels = levels
        ) %>%
            ggplot2::ggplot(ggplot2::aes(
                elements,
                ppm,
                group = ID,
                color = !!quo_group
            )) +
            ggplot2::geom_line(alpha = 0.2) +
            ggplot2::geom_line(
                data = data2,
                ggplot2::aes(elements, Median, group = XXX),
                alpha = 0.6,
                color = "grey70",
                size = 2
            ) +
            ggplot2::geom_line(
                data = data1,
                ggplot2::aes(
                    elements,
                    Median,
                    color = !!quo_group,
                    group = !!quo_group
                ),
                size = 2.5
            )
    } else {
        spider_p <- By_Gather_Ribbon(
            data = data,
            group = group,
            elements = elements,
            levels = levels
        ) %>%
            ggplot2::ggplot(ggplot2::aes(group = ID, color = !!quo_group)) +
            ggplot2::geom_ribbon(
                ggplot2::aes(
                    ymin = min,
                    ymax = max,
                    x = elements,
                    fill = !!quo_group,
                    color = !!quo_group
                ),
                linetype = "blank",
                alpha = 0.2
            ) +
            ggplot2::geom_line(
                data = data2,
                ggplot2::aes(elements, Median, group = XXX, color = XXX),
                alpha = 0.6,
                size = 1
            ) +
            ggplot2::geom_line(
                ggplot2::aes(
                    elements,
                    median,
                    color = !!quo_group,
                    group = !!quo_group
                ),
                size = 2
            )
    }

    spider_p <- spider_p +
        ggplot2::facet_wrap(quo_group, ncol = ncol) +
        ggplot2::scale_y_log10(labels = prettyNum) +
        viridis::scale_fill_viridis(discrete = TRUE) +
        viridis::scale_color_viridis(discrete = TRUE) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            ...
        ) +
        ggplot2::labs(x = "", y = "")

    return(spider_p)
}
