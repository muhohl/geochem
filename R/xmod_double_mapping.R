#' XMOD Double Maps
#'
#' Function that plots two XMOD maps, one with the selected minerals
#' and one with the remaining minerals. This allows to highlight paragenetic
#' relationships.
#'
#' @param xmod_dfs_list
#' A list containing all the XMOD dataframes to plot.
#' @param cutoff
#' Minimum percentage a mineral must occur in the map with remaining minerals.
#' @param sel_min
#' Vector containing the selected minerals.
#' @param sel_min_wrap
#' Name of the selected minerals. Defaults to Ore.
#' @param gangue_min_wrap
#' Name of the remaining minerals. Defaults to Gangue.
#' @param color_sel_min
#' Color scale for the selected minerals. Default is the viridis color scale.
#' If color provided to my_sel_colors this one needs not to be changed.
#' @param color_gangue_min
#' Color scale for the remaining minerals. Default is the plasma virdis color scale.
#' If color provided to my_gangue_colors this one needs not to be changed.
#' @param my_sel_colors
#' Custom color values for the selected minerals.
#' @param my_gangue_colors
#' Custom color values for the remaining minerals.
#' @param name_append
#' Suffix to append to the plot file.
#' @param directory
#' Directory created within the current working environment in which the plots
#' are saved. !Warning! This might override existing data.
#' @param filetype
#' Filetype ending. Default is ".png".
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import magrittr
xmod_double_mapping <- function(xmod_dfs_list,
                                cutoff = 0.2,
                                sel_min,
                                sel_min_wrap = 'Selected',
                                gangue_min_wrap = 'Gangue',
                                color_sel_min = ggplot2::scale_color_viridis_d,
                                color_gangue_min = ggplot2::scale_color_viridis_d,
                                my_sel_colors = FALSE,
                                my_gangue_colors = FALSE,
                                name_append = NULL,
                                directory = './XMOD_Plots',
                                filetype = '.png') {

    dir.create(dirname(paste0(directory, '/test.png')), showWarnings = FALSE)

    if (my_sel_colors[[1]] != FALSE) {
        color_sel_min <- ggplot2::scale_color_manual(name = sel_min_wrap,
                                            values = my_sel_colors)
    }
    else {
        color_sel_min <- color_sel_min(sel_min_wrap)
    }

    if (my_gangue_colors[[1]] != FALSE) {
        color_gangue_min <- ggplot2::scale_color_manual(name = gangue_min_wrap,
                                               values = my_gangue_colors)
    }
    else {
        color_gangue_min <- color_gangue_min(gangue_min_wrap,
                                             option = "plasma",
                                             guide = ggplot2::guide_legend(override.aes =
                                                                  list(size = 3,
                                                                       alpha = 1)))
    }


    for (i in 1:length(xmod_dfs_list)) {

        MH_STA <- xmod_dfs_list[[i]]
        MH_STA <- MH_STA %>%
            dplyr::mutate(Mineral_Class = dplyr::if_else(Mineral_Name %in% sel_min,
                                           sel_min_wrap, gangue_min_wrap))

        MH_STA_summarised <- MH_STA %>%
            dplyr::group_by(Mineral_Name) %>%
            dplyr::summarise(`Wt%` = sum(`Wt%`)) %>%
            dplyr::arrange(dplyr::desc(`Wt%`)) %>%
            dplyr::mutate(cumsum = cumsum(`Wt%`)) %>%
            dplyr::filter(`Wt%` > cutoff) %>%
            dplyr::filter(Mineral_Name != "_Aluminum")

        MH_STA_subset <- MH_STA %>%
            dplyr::filter(Mineral_Name %in% MH_STA_summarised$Mineral_Name &
                       !Mineral_Name %in% sel_min)

        MH_STA_ore <- MH_STA %>%
            dplyr::filter(Mineral_Name %in% sel_min)

        covered_data <- round((nrow(MH_STA_ore) +
                                   nrow(MH_STA_subset)) / nrow(MH_STA) * 100,
                              digits = 2)
        max_xaxis <- max(MH_STA$Center_X)
        max_yaxis <- max(MH_STA$Center_Y)

        ann_text_covered <- data.frame(Center_X = (max_xaxis - 3000),
                                       Center_Y = (max_yaxis - 500),
                                       Mineral_Class =
                                           factor(sel_min_wrap,levels =
                                                      c(sel_min_wrap,
                                                        gangue_min_wrap)))

        ann_text_cutoff <- data.frame(Center_X = (max_xaxis - 1500),
                                      Center_Y = (max_yaxis - 1500),
                                      Mineral_Class =
                                          factor(sel_min_wrap,levels =
                                                     c(sel_min_wrap,
                                                       gangue_min_wrap)))

        p <- ggplot2::ggplot() +
            ggplot2::geom_point(data = MH_STA_subset, ggplot2::aes(Center_X, Center_Y,
                                                 color = Mineral_Name),
                       size = 0.3,
                       shape = 15) +
            color_gangue_min +
            ggnewscale::new_scale_color() +
            ggplot2::geom_point(data = MH_STA_ore, ggplot2::aes(Center_X, Center_Y,
                                              color = Mineral_Name),
                       size = 0.5,
                       shape = 15) +
            ggplot2::coord_fixed() +
            color_sel_min +
            ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 3))) +
            ggplot2::geom_text(data = ann_text_covered, ggplot2::aes(Center_X, Center_Y),
                      label = paste0("Recov. Data: ", covered_data, "%")) +
            ggplot2::geom_text(data = ann_text_cutoff, ggplot2::aes(Center_X, Center_Y),
                      label = paste0("Cutoff: >", cutoff, "wt%")) +
            ggplot2::facet_wrap(~ Mineral_Class) +
            ggplot2::theme_void() +
            ggplot2::labs(x = '', y = '') +
            ggplot2::theme(legend.position="bottom", legend.box="verical",
                  legend.direction = "horizontal",
                  legend.margin = ggplot2::margin(),
                  text = ggplot2::element_text(size = 12))

        ggplot2::ggsave(filename = paste0(directory,
                                 '/',
                                 xmod_dfs_list[[i]][[1, 'Sample']],
                                 '_Double',
                                 name_append,
                                 filetype),
               plot = p)
    }
}


