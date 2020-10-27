spider_plot <- function(data,
                        group,
                        columns,
                        levels,
                        ribbon = TRUE,
                        ncol = NULL) {

    By_Group <- function(data, group, columns, levels) {
        quo_group <-  ggplot2::sym(group)
        data <- data %>% dplyr::select(columns)
        ncols <- ncol(data)
        data %>%
            tidyr::pivot_longer(2:ncols, names_to = "elements", values_to = "ppm") %>%
            dplyr::group_by(!! quo_group, elements) %>%
            dplyr::summarise(Median = stats::median(ppm, na.rm = TRUE)) %>%
            dplyr::mutate(elements = factor(elements,
                                     levels = levels))
    }

    By_Gather <- function(data, columns, levels) {
        data <- data %>% dplyr::select(columns)
        ncols <- ncol(data)
        data %>%
            dplyr::mutate(ID = 1: dplyr::n()) %>%
            tidyr::pivot_longer(2:ncols, names_to = "elements", values_to = "ppm") %>%
            dplyr::mutate(elements = factor(elements,
                                     levels = levels))
    }

    By_Gather_Ribbon <- function(data, group, columns, levels) {
        quo_group <- dplyr::sym(group)
        By_Gather(data = data,
                  columns = columns,
                  levels = levels) %>%
            group_by(elements, !! quo_group) %>%
            summarise(min = quantile(ppm, na.rm = TRUE)[[2]],
                      max = quantile(ppm, na.rm = TRUE)[[4]],
                      median = median(ppm, na.rm = TRUE)) %>%
            mutate(ID = 1:n())
    }

    quo_group <- sym(group)

    data1 <- By_Group(data = data,
                      group = group,
                      columns = columns,
                      levels = levels)

    data2 <- data1
    data2 <- rename(data2, "XXX" = 1)

    if (ribbon == FALSE) {

        By_Gather(data = data,
                  columns = columns,
                  levels = levels) %>%
            ggplot(aes(elements,
                       ppm,
                       group = ID,
                       color = !! quo_group)) +
            geom_line(alpha = 0.2) +
            geom_line(data = data2, aes(elements,
                                        Median,
                                        group = XXX),
                      alpha = 0.6,
                      color = "grey70",
                      size = 2) +
            geom_line(data = data1, aes(elements,
                                        Median,
                                        color = !! quo_group,
                                        group = !! quo_group),
                      size = 2.5) +
            facet_wrap(quo_group, ncol = ncol) +
            scale_y_log10(labels = prettyNum) +
            scale_fill_viridis(discrete = TRUE) +
            scale_color_viridis(discrete = TRUE) +
            theme_dark() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(x = "")

    } else {

        By_Gather_Ribbon(data = data,
                         group = group,
                         columns = columns,
                         levels = levels) %>%
            ggplot(aes(group = ID,
                       color = !! quo_group)) +
            geom_ribbon(aes(ymin = min,
                            ymax = max,
                            x = elements,
                            fill = !! quo_group,
                            color = !! quo_group),
                        linetype = "blank",
                        alpha = 0.2) +
            geom_line(data = data2, aes(elements,
                                        Median,
                                        group = XXX,
                                        color = XXX),
                      alpha = 0.6,
                      size = 1) +
            geom_line(aes(elements,
                          median,
                          color = !! quo_group,
                          group = !! quo_group),
                      size = 2) +
            facet_wrap(quo_group, ncol = ncol) +
            scale_y_log10(labels = prettyNum) +
            scale_fill_viridis(discrete = TRUE) +
            scale_color_viridis(discrete = TRUE) +
            theme_dark() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(x = "", y = "")
    }
}
