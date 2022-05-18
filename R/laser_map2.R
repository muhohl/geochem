
#' Plot for Elemental Map
#'
#' @param data
#' @param columns
#' @param option
#' @param trans
#' @param breaks
#' @param labels
#' @param label_start
#' @param LETTERS
#' @param unit
#' @param pca_rec
#' @param option_Temp
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import magrittr
laser_map2 <- function(data,
                       columns,
                       option = "turbo",
                       trans = "log",
                       breaks = c(10^(-3:5)),
                       labels = scales::label_scientific(),
                       plot_label_start = "B",
                       plot_label = LETTERS,
                       unit = "[ppm]",
                       pca_rec = NA,
                       option_Temp = "D") {
    pl.maps <- list()
    j <- which(plot_label_start == plot_label)
    i <- 1


    # Check for X,Y coordinates name and change them to lower case
    for (k in names(data)) {
        if (k == "X") {
            data <- dplyr::rename_with(data, tolower, c("X"))
        }
        if (k == "Y") {
            data <- dplyr::rename_with(data, tolower, c("Y"))
        }
    }

    for (element in names(data)[columns]) {

        if (is.na(plot_label_start)|plot_label_start == "") {
            plot_enumerator <- ""
        } else {
            plot_enumerator <- paste0(LETTERS[j], ") ")
        }

        p.map <- ggplot2::ggplot(data = data,
                                 ggplot2::aes(x, y,
                                     fill = !! ggplot2::sym(element))) +
            ggplot2::geom_raster(interpolate = TRUE) +
            ggplot2::coord_fixed(ratio = 1) +
            ggplot2::scale_y_discrete(expand = c(0,0)) +
            ggplot2::scale_x_discrete(expand = c(0,0)) +
            ggplot2::labs(fill = "",
                          y = "",
                          x = "") +
            ggplot2::theme_void() +
            ggplot2::theme(panel.border = ggplot2::element_blank(),
                  panel.background = ggplot2::element_rect(fill = "black"),
                  plot.margin = ggplot2::margin(r = 2, b = 2, l = 2),
                  text = ggplot2::element_text(family = "serif",
                                      size = 16)) +
            ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = ggplot2::unit(0.6, "lines"),
                                         barheight = ggplot2::unit(6, "lines"),
                                         ticks.colour = "black",
                                         frame.colour = "black"))

        if (stringr::str_detect(element, "/")) {
            p.map <- p.map +
                ggplot2::scale_fill_gradient2(trans = "log",
                                              low = "#001096",
                                              high = "#E60000",
                                              mid = "grey80",
                                              breaks = breaks,
                                              expand = c(0,0),
                                              labels = scales::label_number(accuracy = 0.01)) +
                ggplot2::ggtitle(paste0(plot_enumerator, element))

            }
        else if (stringr::str_detect(element, "PC")) {

            if (class(pca_rec) == "recipe") {
                expl_var_all <- pca_rec %>%
                    recipes::tidy(id = "pca", type = "variance") %>%
                    dplyr::filter(terms == "percent variance") %>%
                    dplyr::pull(value)
                sel_pc_new <- stringr::str_extract(element, "\\d\\d|\\d")
                expl_var <- round(expl_var_all[as.numeric(sel_pc_new)], 2)
                }

            p.map <- p.map +
                scico::scale_fill_scico(palette = "vikO",
                                        midpoint = 0) +
                ggplot2::ggtitle(paste0(plot_enumerator, element,
                                        sprintf(" - %0.1f%% expl. var.", expl_var)))
            }

        else if (stringr::str_detect(element, "kNN")) {
            p.map <- p.map +
                ggplot2::scale_fill_continuous(name = "Cluster") +
                ggplot2::ggtitle(paste0(plot_enumerator, element))
            }

        else if (stringr::str_detect(element, "Temperature")) {
            p.map <- p.map +
                ggplot2::scale_fill_viridis_c(option = option_Temp) +
                ggplot2::ggtitle(paste0(plot_enumerator, element, " [°C]"))
        }
        else {
            p.map <- p.map +
                    ggplot2::scale_fill_viridis_c(option = option,
                                                  trans = trans,
                                                  breaks = breaks, # Not ideal because breaks are now set for log trans
                                                  labels = labels) +
                    ggplot2::ggtitle(paste0(plot_enumerator, element, " ", unit)) # Take the at the beginning of the for loop, since it constant no matter which if loop is entered
            }

        pl.maps[[i]] <- p.map
        i <- i+1
        j <- j+1
        }

    return(pl.maps)
    }
