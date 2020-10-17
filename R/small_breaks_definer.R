#' Helper functions that defines legend breaks for the Laser Maps.
#' Only used for linear scales.
#'
#' @param min_scale
#' Minimum of the scale
#' @param mid_scale
#' Middle of the scale
#' @param max_scale
#' Maximum of the scale
#' @param small_scale
#' Boolean Value defining if a small scale is necassary
#' @param min_ppm
#' Minimum value for the column
#' @param max_ppm
#' Maximum value for the column
#' @param smallest_scale_I
#' Defines where the breaks in the legend occur
#' @param smallest_scale_II
#' Defines where the breaks in the legend occur
#' @param smallest_scale_III
#' Defines where the breaks in the legend occur
#'
#' @return
#' @export
#'
#' @examples
small_breaks_definer <- function(min_scale,
                                 mid_scale,
                                 max_scale,
                                 small_scale,
                                 min_ppm,
                                 max_ppm,
                                 smallest_scale_I,
                                 smallest_scale_II,
                                 smallest_scale_III) {

    breaks <- c(geochem::round_up(min_scale*max_ppm),
                geochem::round_up(mid_scale*max_ppm),
                geochem::round_up(max_scale*max_ppm),
                max_ppm)

    if (min_ppm > min_scale*max_ppm) {
        breaks <- c(geochem::round_up(min_scale*max_ppm + min_ppm),
                    geochem::round_up(mid_scale*max_ppm + min_ppm),
                    geochem::round_up(max_scale*max_ppm + min_ppm),
                    max_ppm)
    }

    small_scale <- geochem::double_values_checker(breaks_fun = breaks,
                                                  small_scale = FALSE)

    if (small_scale) {
        if (min_ppm > pretty(min_scale*max_ppm)[2] &
            min_ppm < pretty(mid_scale*max_ppm)[2]) {
            breaks <- c(pretty(mid_scale*max_ppm)[2],
                        max_ppm)
            small_scale <- FALSE
            smallest_scale_I <- TRUE
        }
        if (min_ppm > pretty(max_scale*max_ppm)[2]) {
            breaks <- c(min_ppm,
                        max_ppm)
            smallest_scale_II <- TRUE
            small_scale <- FALSE
        }
        if (min_ppm < pretty(max_scale*max_ppm)[2] & !smallest_scale_I) {
            breaks <- c(max_scale*max_ppm,
                        max_ppm)
            smallest_scale_III <- TRUE
            small_scale <- FALSE
        }
    } else {
        small_scale <- TRUE
    }

    return(list('breaks' = breaks,
                'small_scale' = small_scale,
                'smallest_scale_I' = smallest_scale_I,
                'smallest_scale_II' = smallest_scale_II,
                'smallest_scale_III' = smallest_scale_III))
}
