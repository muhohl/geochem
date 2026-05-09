#' Create list of XMOD files to plot
#'
#' Loads all CSV files in the specified directory whose names match
#' `file_pattern`. The returned list is used by [xmod_df_wrangler()] to load
#' the data frames into the current environment.
#'
#' @param file_pattern Character scalar; common name pattern of the CSV files
#'   to include.
#' @param directory Path to the directory containing the XMOD CSV files.
#'
#' @return A character vector of matching file names.
#' @export
#'
#' @examples
#' xmod_samples_list('XMOD_Sample', './/XMOD//Samples')
xmod_samples_list <- function(file_pattern, directory) {
    intersect(
        list.files(path = paste0(getwd(), directory), pattern = "*\\.csv$"),
        list.files(path = paste0(getwd(), directory), pattern = file_pattern)
    )
}


#' Wrangle XMOD data frames into a plottable format
#'
#' Reads XMOD modal `.xls` files and coordinate `.csv` files from `directory`,
#' joins them, and attaches wt% values from the modal data. Returns a list of
#' data frames ready for plotting.
#'
#' @param directory Directory (relative to `getwd()`) where the XMOD files are
#'   located.
#' @param xls_common_pattern Pattern matching the modal data `.xls` files.
#' @param csv_common_pattern Pattern matching the coordinate `.csv` files.
#' @param csv_list_to_plot Either `FALSE` (default) to auto-detect CSV files
#'   via [xmod_samples_list()], or a pre-built character vector of file names.
#'
#' @return A list of data frames, one per sample.
#' @export
#'
#' @examples
#' \dontrun{
#' xmod_df_wrangler("./XMOD", "XMOD_", "Coords_")
#' }
#'
#' @import magrittr
xmod_df_wrangler <- function(directory,
                             xls_common_pattern,
                             csv_common_pattern,
                             csv_list_to_plot = FALSE) {

    xls_files <- list()
    all_xls_in_directory <-
        intersect(list.files(path = paste0(getwd(), directory),
                             pattern = "*\\.xls$"),
                  list.files(path = paste0(getwd(), directory),
                             pattern = xls_common_pattern))

    for (i in 1:length(all_xls_in_directory)) {
        xls_files[[i]] <- readxl::read_xls(
            paste0(getwd(), directory, '/', all_xls_in_directory[i]),
            1,
            skip = 3
        )
    }

    Modal_Data <- plyr::join_all(xls_files, by = 'Mineral', type = 'full')

    Modal_Data_Wt <- Modal_Data[stringr::str_detect(names(Modal_Data), 'Wt%')]
    Modal_Data_Wt <- cbind(Modal_Data[1], Modal_Data_Wt)

    Modal_Data_Trans <- Modal_Data_Wt %>%
        tidyr::gather(Sample, Wt, 2:ncol(Modal_Data_Wt)) %>%
        dplyr::distinct() %>%
        stats::na.omit() %>%
        tidyr::spread(Mineral, Wt)
    Modal_Data_Trans <- Modal_Data_Trans %>%
        tidyr::gather(MineralName, Wt, 2:ncol(Modal_Data_Trans)) %>%
        tidyr::spread(Sample, Wt)

    if (csv_list_to_plot[[1]] == FALSE) {
        all_csv_in_directory <- xmod_samples_list(
            file_pattern = csv_common_pattern,
            directory    = directory
        )
    } else {
        all_csv_in_directory <- csv_list_to_plot
    }

    all_df <- list()
    for (i in 1:length(all_csv_in_directory)) {

        MH_STA <- readr::read_csv(
            paste0(getwd(), directory, '/', all_csv_in_directory[i]),
            skip = 4
        )
        MH_STA <- MH_STA %>%
            dplyr::rename(
                Center_X    = `Xray_X`,
                Center_Y    = `Xray_Y`,
                Mineral_Name = MineralName
            )

        Isolated_Sample <- Modal_Data_Trans[i + 1] %>%
            dplyr::rename(`Wt%` = 1)

        all_df[[i]] <- dplyr::left_join(
            MH_STA,
            cbind(
                Modal_Data_Trans[1] %>% dplyr::rename(Mineral_Name = 1),
                Isolated_Sample
            )
        )
    }
    return(all_df)
}


#' XMOD double maps
#'
#' Plots two XMOD maps side-by-side for each sample: one showing
#' `sel_min` (e.g. ore minerals) and one showing the remaining minerals above
#' `cutoff` wt%. Saves each plot to `directory`.
#'
#' @param xmod_dfs_list A list of XMOD data frames, e.g. from
#'   [xmod_df_wrangler()].
#' @param cutoff Minimum wt% a mineral must reach to appear in the gangue map
#'   (default `0.2`).
#' @param sel_min Character vector of selected mineral names (e.g. ore
#'   minerals).
#' @param sel_min_wrap Legend title for the selected minerals (default
#'   `"Selected"`).
#' @param gangue_min_wrap Legend title for the remaining minerals (default
#'   `"Gangue"`).
#' @param color_sel_min Colour scale function for selected minerals. Defaults
#'   to [ggplot2::scale_color_viridis_d()].
#' @param color_gangue_min Colour scale function for remaining minerals.
#'   Defaults to [ggplot2::scale_color_viridis_d()].
#' @param my_sel_colors Optional named colour vector for selected minerals.
#' @param my_gangue_colors Optional named colour vector for remaining minerals.
#' @param name_append Optional suffix appended to each output file name.
#' @param directory Output directory for saved plots (default
#'   `"./XMOD_Plots"`).
#' @param filetype File extension for saved plots (default `".png"`).
#'
#' @return Invisibly `NULL`; called for its side effect of saving plot files.
#' @export
#'
#' @examples
#' \dontrun{
#' xmod_double_mapping(xmod_list, sel_min = c("Pyrite", "Chalcopyrite"))
#' }
#'
#' @import magrittr
xmod_double_mapping <- function(xmod_dfs_list,
                                cutoff           = 0.2,
                                sel_min,
                                sel_min_wrap     = 'Selected',
                                gangue_min_wrap  = 'Gangue',
                                color_sel_min    = ggplot2::scale_color_viridis_d,
                                color_gangue_min = ggplot2::scale_color_viridis_d,
                                my_sel_colors    = FALSE,
                                my_gangue_colors = FALSE,
                                name_append      = NULL,
                                directory        = './XMOD_Plots',
                                filetype         = '.png') {

    dir.create(dirname(paste0(directory, '/test.png')), showWarnings = FALSE)

    if (my_sel_colors[[1]] != FALSE) {
        color_sel_min <- ggplot2::scale_color_manual(
            name   = sel_min_wrap,
            values = my_sel_colors
        )
    } else {
        color_sel_min <- color_sel_min(sel_min_wrap)
    }

    if (my_gangue_colors[[1]] != FALSE) {
        color_gangue_min <- ggplot2::scale_color_manual(
            name   = gangue_min_wrap,
            values = my_gangue_colors
        )
    } else {
        color_gangue_min <- color_gangue_min(
            gangue_min_wrap,
            option = "plasma",
            guide  = ggplot2::guide_legend(
                override.aes = list(size = 3, alpha = 1)
            )
        )
    }

    for (i in 1:length(xmod_dfs_list)) {

        MH_STA <- xmod_dfs_list[[i]] %>%
            dplyr::mutate(Mineral_Class = dplyr::if_else(
                Mineral_Name %in% sel_min, sel_min_wrap, gangue_min_wrap
            ))

        MH_STA_summarised <- MH_STA %>%
            dplyr::group_by(Mineral_Name) %>%
            dplyr::summarise(`Wt%` = sum(`Wt%`)) %>%
            dplyr::arrange(dplyr::desc(`Wt%`)) %>%
            dplyr::mutate(cumsum = cumsum(`Wt%`)) %>%
            dplyr::filter(`Wt%` > cutoff) %>%
            dplyr::filter(Mineral_Name != "_Aluminum")

        MH_STA_subset <- MH_STA %>%
            dplyr::filter(
                Mineral_Name %in% MH_STA_summarised$Mineral_Name &
                    !Mineral_Name %in% sel_min
            )

        MH_STA_ore <- MH_STA %>%
            dplyr::filter(Mineral_Name %in% sel_min)

        covered_data <- round(
            (nrow(MH_STA_ore) + nrow(MH_STA_subset)) / nrow(MH_STA) * 100,
            digits = 2
        )
        max_xaxis <- max(MH_STA$Center_X)
        max_yaxis <- max(MH_STA$Center_Y)

        ann_text_covered <- data.frame(
            Center_X     = max_xaxis - 3000,
            Center_Y     = max_yaxis - 500,
            Mineral_Class = factor(sel_min_wrap,
                                   levels = c(sel_min_wrap, gangue_min_wrap))
        )
        ann_text_cutoff <- data.frame(
            Center_X     = max_xaxis - 1500,
            Center_Y     = max_yaxis - 1500,
            Mineral_Class = factor(sel_min_wrap,
                                   levels = c(sel_min_wrap, gangue_min_wrap))
        )

        p <- ggplot2::ggplot() +
            ggplot2::geom_point(
                data  = MH_STA_subset,
                ggplot2::aes(Center_X, Center_Y, color = Mineral_Name),
                size  = 0.3,
                shape = 15
            ) +
            color_gangue_min +
            ggnewscale::new_scale_color() +
            ggplot2::geom_point(
                data  = MH_STA_ore,
                ggplot2::aes(Center_X, Center_Y, color = Mineral_Name),
                size  = 0.5,
                shape = 15
            ) +
            ggplot2::coord_fixed() +
            color_sel_min +
            ggplot2::guides(colour = ggplot2::guide_legend(
                override.aes = list(size = 3)
            )) +
            ggplot2::geom_text(
                data  = ann_text_covered,
                ggplot2::aes(Center_X, Center_Y),
                label = paste0("Recov. Data: ", covered_data, "%")
            ) +
            ggplot2::geom_text(
                data  = ann_text_cutoff,
                ggplot2::aes(Center_X, Center_Y),
                label = paste0("Cutoff: >", cutoff, "wt%")
            ) +
            ggplot2::facet_wrap(~ Mineral_Class) +
            ggplot2::theme_void() +
            ggplot2::labs(x = '', y = '') +
            ggplot2::theme(
                legend.position  = "bottom",
                legend.box       = "vertical",
                legend.direction = "horizontal",
                legend.margin    = ggplot2::margin(),
                text             = ggplot2::element_text(size = 12)
            )

        ggplot2::ggsave(
            filename = paste0(
                directory, '/',
                xmod_dfs_list[[i]][[1, 'Sample']],
                '_Double', name_append, filetype
            ),
            plot = p
        )
    }
}
