#' Create list of XMOD files to plot
#'
#' The function is loading all the the data frames in the specified directory that end with
#' .csv in a list. The list is used in the following function to load the
#' data frames into the current environment.
#'
#' @param file_pattern
#' String with the common name pattern of all csvs in the directory that should be plotted.
#' @param directory
#' Location of the directory which contains the XMOD csv's.
#'
#' @return
#' @export
#'
#' @examples
#' xmod_samples_list('XMOD_Sample', './/XMOD//Samples')
xmod_samples_list <- function(file_pattern,
                              directory) {


    all_csv_in_directory <-
        intersect(list.files(path = paste0(getwd(), directory),
                             pattern = "*\\.csv$"),
                  list.files(path = paste0(getwd(), directory),
                             pattern = file_pattern))
    return(all_csv_in_directory)
}
