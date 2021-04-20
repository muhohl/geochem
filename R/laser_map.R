#' Laser Maps Plot
#'
#' Function that creates laser maps of selected elements. The function creates
#' a list containing the individual laser plots. The list can than used to
#' plot all the maps at once via ggpubr::ggarrange or individual plots can
#' be plotted by selecting single elements out of the list.
#'
#' @param data
#' Laser map data frame. If PCA are plotted the PC columns must be merged with
#' the laser map data frame.
#' @param selected_elements
#' Vector containing the selected elements and PC's will be plotted.
#' @param Log_Trans
#' Data set containing a column called "Log" which indicates if a map should be
#' log transformed. If not specified all elements are log transformed.
#' @param pcobj
#' PCA object created by stats::prcomp.
#' @param sel_pc
#' Selecting the principal components from the pcobj to calculate the explained
#' variance shown by the laser map.
#' @param unit_title
#' Specify the unit of the laser map for the plot title as a string. Has no
#' effect on the values.
#' @param fontsize
#' Fontsize used in the plot.
#' @param font
#' Font type, options are "serif", "sans" and "mono".
#' @param labels
#' Argument for the scale notation. See ?scales:: for more information.
#' @param ...
#' Options for viridis color scale. Includes "A", "B", "C", "D".
#' See ?ggplot2::scale_fill_viridis for more information.
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import magrittr
laser_map <- function(data,
                     selected_elements,
                     Log_Trans = FALSE,
                     pcobj = NA,
                     sel_pc = NA,
                     unit_title = "(ppm)",
                     fontsize = 14,
                     font = "serif",
                     labels = scales::label_scientific(),
                     ...) {

    # Create empty vectors for the following for loop
    plot_list <- list()
    plot_list_new <- list()

    breaks <- exp(seq(log(0.01), log(1000000), length.out = 9))


    # Check for X,Y coordinates name and change them to lower case
    for (i in names(data)) {
        if (i == "X") {
            data <- dplyr::rename_with(data, tolower, c("X"))
        }
        if (i == "Y") {
            data <- dplyr::rename_with(data, tolower, c("Y"))
        }
    }


    # The plotting for loop!
    for (i in 1:length(selected_elements)) {

        # define variable at the beginning of each iteration
        element <- colnames(data[selected_elements[i]])
        min_ppm <- min(data[selected_elements[i]])
        max_ppm <- max(data[selected_elements[i]])
        min_log <- log(min_ppm)
        max_log <- log(max_ppm)
        range_log <- max_log - min_log


        # Look up which elements should be log transformed
        if (typeof(Log_Trans) == "logical") {
            if (Log_Trans == FALSE) {
                trans_arg <- c("log")
            }
        } else {
            trans_arg <- Log_Trans %>%
                dplyr::filter(!! dplyr::sym(names(Log_Trans[1])) == element) %>%
                dplyr::pull(2)
        }

        small_scale <- FALSE
        smallest_scale <- FALSE

        small_breaks_list <- list('breaks' = NA,
                                  'small_scale' = small_scale,
                                  'smallest_scale_I' = FALSE,
                                  'smallest_scale_II' = FALSE,
                                  'smallest_scale_III' = FALSE)

        # Create the base plot
        p1 <- data %>%
            ggplot2::ggplot(ggplot2::aes(x, y,
                       fill = !! ggplot2::sym(element))) +
            ggplot2::geom_raster(interpolate = TRUE) +
            ggplot2::theme_bw() +
            ggplot2::theme(panel.border = ggplot2::element_blank(),
                           text = ggplot2::element_text(family = font,
                                                        size = fontsize)) +
            ggplot2::coord_fixed(ratio = 1) +
                  #aspect.ratio = 1) +
            ggplot2::scale_y_discrete(expand = c(0,0)) +
            ggplot2::scale_x_discrete(expand = c(0,0)) +
            ggplot2::labs(fill = "",
                 y = "",
                 x = "") +
            ggplot2::ggtitle(paste(element, unit_title))

        # From here onwards is mostly scale handling
        if (!stringr::str_detect(element, 'PC')) {

            # If statements that decide if log transformed or not
            if (trans_arg == 'identity') {

                breaks_one <- FALSE
                # define breaks for the scale
                small_breaks_list[[1]] <- c(pretty(min_ppm)[2], # offer if statement for
                                            pretty(0.25*max_ppm)[2], # pretty numbers
                                            pretty(0.5*max_ppm)[2],
                                            pretty(0.75*max_ppm)[2])

                if (small_breaks_list[[1]][4] > max_ppm) {
                    small_breaks_list[[2]] <- TRUE
                }



                # check for duplicate or larger values in the scale, if so reduce the number of
                # breaks to three
                small_breaks_list[[2]] <- geochem::double_values_checker(breaks_fun = small_breaks_list[[1]],
                                                                         small_scale = small_breaks_list[[2]])

                if (small_breaks_list[[2]]) {
                    small_breaks_list <- geochem::small_breaks_definer(min_scale = 0.25,
                                                                       mid_scale = 0.5,
                                                                       max_scale = 0.75,
                                                                       small_scale = small_breaks_list[[2]],
                                                                       min_ppm = min(data[selected_elements[i]]),
                                                                       max_ppm = max(data[selected_elements[i]]),
                                                                       smallest_scale_I = FALSE,
                                                                       smallest_scale_II = FALSE,
                                                                       smallest_scale_III = FALSE)
                }

                # Add the color scale and title according to log transformation or not
                p1 <- p1 +
                    ggplot2::scale_fill_viridis_c(option = ...,
                                         trans = trans_arg,
                                         limits = c(min_ppm, max_ppm),
                                         breaks = small_breaks_list[[1]],
                                         expand = c(0,0),
                                         labels = labels) +
                    ggplot2::ggtitle(paste(element, unit_title))

                min_log <- min_ppm
                max_log <- max_ppm
                range_log <- max_log-min_log
                breaks_in_element <- small_breaks_list[[1]]
            }

            if (trans_arg == 'log') {

                breaks_one <- FALSE
                breaks_in_element <- c()

                for (brk in breaks) {
                    if (brk > min_ppm & brk < max_ppm) {
                        breaks_in_element <- append(breaks_in_element, brk)
                    }
                }

                if (length(breaks_in_element) == 1) {
                    breaks_in_element <- append(breaks_in_element, min_ppm, after = 0)
                    breaks_one <- TRUE
                }

                p1 <- p1 +
                    ggplot2::scale_fill_viridis_c(option = ...,
                                         trans = trans_arg,
                                         limits = c(min_ppm, max_ppm),
                                         breaks = breaks_in_element,
                                         expand = c(0,0),
                                         labels = labels) +
                    ggplot2::ggtitle(paste(element, unit_title, ' - log')) #+
                #theme(text = element_text(family = 'serif')) # make it into if statement
                # if true than lapply to the complete list at the end of the program, like
                # the margin is applied

                if (breaks_one) {
                    breaks_in_element <- breaks_in_element[2]
                }
            }
        }

        # PCA legend can't be logarithmic due to negative values.
        # But the legend is divergent and the center value (0) is not the
        # center of the legend.
        if (stringr::str_detect(element, 'PC')) {

            # get the explained variance for the selected principal components
            expl_var <- (100 * pcobj$sdev[sel_pc]^2/sum(pcobj$sdev^2))[as.numeric(stringr::str_extract(element, '\\d\\d|\\d'))]

            small_breaks_list[[1]] <- c(min_ppm,
                                        min_ppm/2,
                                        0,
                                        max_ppm/2,
                                        max_ppm)

            p1 <- p1 +
                ggplot2::scale_fill_gradient2(breaks = small_breaks_list[[1]],
                                     expand = c(0,0),
                                     high = 'red',
                                     low = 'cyan',
                                     mid = 'grey35',
                                     labels = labels) +
                ggplot2::ggtitle(paste(element, sprintf('- %0.1f%% explained var.', expl_var)))
        }


        # Resize the legend, so that it is the same size as the plot
        # Code from github: https://stackoverflow.com/questions/19214914/how-can-i-make-the-legend-in-ggplot2-the-same-height-as-my-plot
        # Here it's the answer by Sandy Muspratt; second answer
        plot_list[[i]] <- p1

        # Get the ggplot grob
        gt <- ggplot2::ggplotGrob(plot_list[[i]])

        # Get the legend
        leg <- gtable::gtable_filter(gt, "guide-box")

        # Raster height
        leg[[1]][[1]][[1]][[1]][[1]][[2]]$height <- grid::unit(1, "npc")

        if (trans_arg == "log" & !stringr::str_detect(element, 'PC')) {
            breaks_in_element <- log(breaks_in_element)
        }

        if (length(breaks_in_element) >= 1) {
            # Positions for labels and tick marks - five breaks, therefore, five positions
            pos <- grid::unit.c(grid::unit((breaks_in_element[1]-min_log)/range_log, "npc"))
        }

        for (l in 2:length(breaks_in_element)){
            if (length(breaks_in_element) >= l) {
                pos[l] <- grid::unit((breaks_in_element[l]-min_log)/range_log, "npc")
            }
        }

        if (breaks_one) {
            pos[2] <- pos[1]
            pos[1] <- grid::unit(0.01, "npc")
        }



        # Define the position for the PCA legend, as percentages to their negative and positive extremes
        if (stringr::str_detect(element, 'PC')) {
            pos <- grid::unit.c(grid::unit(0.01,"npc"),
                                grid::unit(abs(min_ppm)/(abs(min_ppm-max_ppm))/2, "npc"),
                                grid::unit(abs(min_ppm)/(abs(min_ppm - max_ppm)), "npc"),
                                grid::unit(abs(max_ppm)/(abs(min_ppm-max_ppm))/2+(abs(min_ppm)/(abs(min_ppm - max_ppm))), "npc"), grid::unit(.99, "npc"))
        }

        # Positions the labels
        leg[[1]][[1]][[1]][[1]][[1]][[3]]$children[[1]]$y <- pos

        # Positions the tick marks
        leg[[1]][[1]][[1]][[1]][[1]][[5]]$y0 <- pos
        leg[[1]][[1]][[1]][[1]][[1]][[5]]$y1 <- pos

        # Legend key height ?
        leg[[1]][[1]][[1]][[1]]$heights <- grid::unit.c(rep(grid::unit(0, "mm"), 3),
                                                        grid::unit(1, "npc"),
                                                        grid::unit(0, "mm"))
        # Legend height
        leg[[1]][[1]]$heights[[3]] <- sum(rep(grid::unit(0, "mm"), 3),
                                          grid::unit(1, "npc"),
                                          grid::unit(0, "mm"))

        # grid.draw(leg)  # Check on heights and y values

        # gtable_show_layout(gt) # Manually locate position of legend in layout
        # save the gtable objects in a new list
        plot_list_new[[i]] <- gtable::gtable_add_grob(gt, leg, t = 7, l = 9)

        # Transform the gtable object back into an ggplot opject
        plot_list_new[[i]] <- ggpubr::as_ggplot(plot_list_new[[i]])
    }
    return(plot_list_new)
}
