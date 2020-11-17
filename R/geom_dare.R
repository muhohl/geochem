
#' geom_dare
#'
#' geoms to draw fields defined by Dare et al. (2014)"
#'
#' @param ...
#' Pass arguments on to geom_text and geom_line
#' @return
#' @export
#'
#' @examples
geom_dare <- function(...) {

    list(ggplot2::layer(data = tibble::tibble(x = c(0.1, 0.4, 0.95, 1, 1.5, 2, 3, 5, 7),
                                         y = c(10, 1000, 10000, 11000, 15000, 17000, 20000, 22000, 24000)),
                   mapping = ggplot2::aes(x, y),
                   geom = ggplot2::GeomLine,
                   stat = "identity",
                   position = "identity",
                   inherit.aes = FALSE,
                   params = list(color = 'black', shape = 1, ...),
                   show.legend = FALSE),
        ggplot2::layer(mapping = ggplot2::aes(x = 10,
                                              y = 10000),
                       geom = ggplot2::GeomText,
                       stat = "identity",
                       position = "identity",
                       inherit.aes = FALSE,
                       params = list(label = "Hydrothermal",
                                     ...),
                       show.legend = FALSE),
        ggplot2::layer(mapping = ggplot2::aes(x = 0.01,
                                              y = 8000),
                       geom = ggplot2::GeomText,
                       stat = "identity",
                       position = "identity",
                       inherit.aes = FALSE,
                       params = list(label = "Magmatic",
                                     ...),
                       show.legend = FALSE))
}
