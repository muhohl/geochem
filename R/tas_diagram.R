#' Total Alkali Silica diagram
#'
#' Template for the popular Total Alkali Silica (TAS) diagram after Le Maitre (1989)
#' The function should be used as a geom in a ggplot.
#'
#' @param fields
#' If TRUE the TAS fields are drawn.
#' @param labels
#' If TRUE the names for the respective field are drawn.
#'
#' @return
#' @export
#'
#' @examples
#' ggplot2::ggplot() +
#'     geochem::tas_diagram()
tas_diagram <- function(fields = TRUE,
                        labels = TRUE) {

    tas <- tibble::tribble(
            ~label, ~x, ~y,
            "p1",   41,  1,
            "p2",   45,  1,
            "p3",   41,  7,
            "p4",   45,  5,
            "p5",   52,  5,
            "p6",   57,  5.9,
            "p7",   63,  7,
            "p8",   69,  8,
            "p9",   69,  13,
            "p10",  61,  13.5,
            "p11",  57.6,11.7,
            "p12",  53,  9.3,
            "p13",  49.4,7.3,
            "p14",  45,  9.4,
            "p15",  48.4,11.5,
            "p16",  52.5,14,
            "p17",  78,  1,
            "p18",  48.065, 16,
            "p19",  63,  1,
            "p20",  57,  1,
            "p21",  52,  1
            )

    # Define lines
    l1 <- tas %>%
        dplyr::filter(label %in% c("p1", "p3", "p14", "p16"))
    l2 <- tas %>%
        dplyr::filter(label %in% c("p2", "p4", "p13", "p10"))
    l3 <- tas %>%
        dplyr::filter(label %in% c("p4", "p5", "p8"))
    l4 <- tas %>%
        dplyr::filter(label %in% c("p18", "p16", "p11", "p7", "p19"))
    l5 <- tas %>%
        dplyr::filter(label %in% c("p15", "p12", "p6", "p20"))
    l6 <- tas %>%
        dplyr::filter(label %in% c("p9", "p8"))
    l7 <- tas %>%
        dplyr::filter(label %in% c("p14", "p13", "p5", "p21"))
    l8 <- tas %>%
        dplyr::filter(label %in% c("p8", "p17"))

    tas_template <- list(
        ggplot2::ylab(expression(paste(Na[2],"O + ", K[2], "O [wt%]"))),
        ggplot2::xlab(expression(paste("Si", O[2], " [wt%]")))
    )
    if (fields) {
        tas_template <- append(tas_template, list(
            ggplot2::geom_line(data = l1, ggplot2::aes(x,y)),
            ggplot2::geom_line(data = l2, ggplot2::aes(x,y)),
            ggplot2::geom_line(data = l3, ggplot2::aes(x,y)),
            ggplot2::geom_line(data = l4, ggplot2::aes(x,y)),
            ggplot2::geom_line(data = l5, ggplot2::aes(x,y)),
            ggplot2::geom_line(data = l6, ggplot2::aes(x,y)),
            ggplot2::geom_line(data = l7, ggplot2::aes(x,y)),
            ggplot2::geom_line(data = l8, ggplot2::aes(x,y))
            )
        )
    }
    if (labels) {
        tas_template <- append(tas_template, list(
            ggplot2::annotate("text", x = 42, y = 3,     label = "picro- \n basalt"),
            ggplot2::annotate("text", x = 43, y = 7.5,   label = "tephrite \n Ol<10%
                     \n basanite \n Ol>10%"),
            ggplot2::annotate("text", x = 48, y = 9.5,   label = "phono- \n tephrite"),
            ggplot2::annotate("text", x = 51.5, y = 12,  label = "tephri- \n phonolite"),
            ggplot2::annotate("text", x = 46, y = 13.5,  label = "fiodite"),
            ggplot2::annotate("text", x = 56, y = 13.5,  label = "phonolite"),
            ggplot2::annotate("text", x = 62, y = 10,    label = "trachyte"),
            ggplot2::annotate("text", x = 72, y = 9,     label = "rhyolite"),
            ggplot2::annotate("text", x = 66.5, y = 4,   label = "dacite"),
            ggplot2::annotate("text", x = 59, y = 3.75,  label = "andesite"),
            ggplot2::annotate("text", x = 53.5, y = 3.5, label = "basaltic \n andesite"),
            ggplot2::annotate("text", x = 46, y = 3.5,   label = "basalt"),
            ggplot2::annotate("text", x = 48.5, y = 5.75,label = "trachy- \n basalt"),
            ggplot2::annotate("text", x = 52, y = 7,     label = "basaltic \n trachy- \n andesite"),
            ggplot2::annotate("text", x = 56, y = 9,     label = "trachy- \n andesite")
            )
        )
    }
    return(tas_template)
}
