#' Wrangles the XMOD data frames into a plotable format.
#'
#' !! Warning this functions overwrites the original data frames!!
#' Loads the data frames if the wt% value is missing. Adds the wt% for
#  each mineral based on the modal file (xls_table).
#'
#' @param directory
#' Directory where the XMOD files are located.
#' @param xls_common_pattern
#' Pattern for the modal data .xls files
#' @param csv_common_pattern
#' Pattern of the xmod files containing the coordinates .csv files
#' @param csv_list_to_plot
#' Can use the geochem::xmod_samples_list function as argument.
#'
#' @return
#' @export
#'
#' @examples
#'
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

    # read xls files into a list
    for (i in 1:length(all_xls_in_directory)) {
        xls_files[[i]] <- readxl::read_xls(paste0(getwd(),
                                  directory,
                                  '/',
                                  all_xls_in_directory[i]),
                                  1,
                                  skip = 3)
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

    # Option to specify the csv which should be plot manually
    # as a list (csv_list).
    if (csv_list_to_plot[[1]] == FALSE) {
        all_csv_in_directory <- geochem::xmod_samples_list(file_name = csv_common_pattern,
                                                           directory = directory)
    } else {
        all_csv_in_directory <- csv_list_II
    }

    #
    all_df <- list()
    for (i in 1:length(all_csv_in_directory)){

        MH_STA <- readr::read_csv(paste0(getwd(),
                                         directory,
                                         '/',
                                         all_csv_in_directory[i]),
                                  skip = 4)
        MH_STA <- MH_STA %>%
            dplyr::rename(Center_X = `Xray_X`,
                          Center_Y = `Xray_Y`,
                          Mineral_Name = MineralName)

        # The csv common pattern needs to overlap with the name of the wt% columns
        # in the modal.xls files.
        Isolated_Sample <-
            Modal_Data_Trans[i+1]

        Isolated_Sample <- Isolated_Sample %>%
            dplyr::rename(`Wt%` = 1)

        all_df[[i]] <- dplyr::left_join(MH_STA, cbind(Modal_Data_Trans[1] %>%
                                                          dplyr::rename(Mineral_Name = 1),
                                                      Isolated_Sample))
    }
    return(all_df)
}
