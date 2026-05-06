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
#' @param ribbon_alpha Transparency of the IQR ribbon (default `0.2`).
#'   Only used when `ribbon = TRUE`.
#' @param linesize Line width for the median lines coloured by group
#'   (default `2`).
#' @param ncol Number of columns in the facet grid.
#' @param facet If `TRUE` (default), each group is shown in its own panel via
#'   [ggplot2::facet_wrap()]. Set to `FALSE` to overlay all groups on a single
#'   panel.
#' @param normalization Chondrite normalisation reference to apply before
#'   plotting. One of `"mcdonough_sun_1995"`, `"boynton_1985"`,
#'   `"nakamura_1974"`, or `"anders_grevesse_1989"`. Set to `NULL` (default)
#'   to plot raw values. Elements not present in the normalisation table are
#'   left unchanged.
#' @param ... Additional arguments passed to [ggplot2::theme()].
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' ggspider(
#'   data = whole_rock_data,
#'   elements = c("rock_type", "La", "Ce", "Nd", "Sm", "Eu", "Gd", "Yb", "Lu"),
#'   group = "rock_type",
#'   normalization = "mcdonough_sun_1995"
#' )
#'
#' @import magrittr
ggspider <- function(
    data,
    elements,
    group,
    levels = elements,
    ribbon = TRUE,
    ribbon_alpha = 0.5,
    linesize = 1.2,
    facet = TRUE,
    ncol = NULL,
    normalization = NULL,
    ...
) {
    chondrite_norm <- tibble::tribble(
        ~element , ~mcdonough_sun_1995 , ~boynton_1985 , ~nakamura_1974 , ~anders_grevesse_1989 ,
        "La"     , 0.237               , 0.310         , 0.329          , 0.2347                ,
        "Ce"     , 0.613               , 0.808         , 0.865          , 0.6032                ,
        "Pr"     , 0.0928              , 0.122         , NA             , 0.0891                ,
        "Nd"     , 0.457               , 0.600         , 0.630          , 0.4524                ,
        "Sm"     , 0.148               , 0.195         , 0.203          , 0.1471                ,
        "Eu"     , 0.0563              , 0.0735        , 0.0770         , 0.0560                ,
        "Gd"     , 0.199               , 0.259         , 0.276          , 0.1966                ,
        "Tb"     , 0.0361              , 0.0474        , NA             , 0.0363                ,
        "Dy"     , 0.246               , 0.322         , 0.343          , 0.2427                ,
        "Ho"     , 0.0546              , 0.0718        , NA             , 0.0556                ,
        "Er"     , 0.160               , 0.210         , 0.225          , 0.1589                ,
        "Tm"     , 0.0247              , 0.0324        , NA             , 0.0242                ,
        "Yb"     , 0.161               , 0.209         , 0.220          , 0.1625                ,
        "Lu"     , 0.0246              , 0.0322        , 0.0339         , 0.0243                ,
        "Y"      , 1.57                , NA            , NA             , 1.56
    )

    valid_norms <- names(chondrite_norm)[-1]
    y_label <- ""

    if (!is.null(normalization)) {
        if (!normalization %in% valid_norms) {
            stop(paste0(
                "'normalization' must be one of: ",
                paste(valid_norms, collapse = ", "),
                ". Use NULL to skip normalisation."
            ))
        }
        elem_cols <- elements[elements != group]
        norm_lookup <- stats::setNames(
            chondrite_norm[[normalization]],
            chondrite_norm$element
        )
        for (el in elem_cols) {
            # strip unit suffixes like _ppm, _ppb, _wt so "La_ppm" matches "La"
            base_el <- sub("_[a-zA-Z%]+$", "", el)
            nv <- norm_lookup[base_el]
            if (!is.na(nv) && nv > 0) {
                data[[el]] <- data[[el]] / nv
            }
        }
        y_label <- "REE/Chondrite"
    }
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
                size = 0.5
            ) +
            ggplot2::geom_line(
                data = data1,
                ggplot2::aes(
                    elements,
                    Median,
                    color = !!quo_group,
                    group = !!quo_group
                ),
                size = linesize
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
                alpha = ribbon_alpha
            ) +
            ggplot2::geom_line(
                data = data2,
                ggplot2::aes(elements, Median, group = XXX),
                alpha = 0.6,
                color = "grey70",
                size = 0.5
            ) +
            ggplot2::geom_line(
                ggplot2::aes(
                    elements,
                    median,
                    color = !!quo_group,
                    group = !!quo_group
                ),
                size = linesize
            )
    }

    if (facet) {
        spider_p <- spider_p + ggplot2::facet_wrap(quo_group, ncol = ncol)
    }

    spider_p <- spider_p +
        ggplot2::scale_y_log10(labels = prettyNum) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            ...
        ) +
        ggplot2::labs(x = "", y = y_label)

    return(spider_p)
}
