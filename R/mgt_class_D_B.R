mgt_class_D_B <- function() {

    Skarn <- tibble(x = c(0.005, 1.5, 1.25, 0.38, 0.145,
                          0.085, 0.047, 0.047, 0.005),
                    y = c(3, 3, 0.95, 0.71, 0.35,
                          0.4, 0.215, 0.12, 0.12))
    Porphyry <- tibble(x = c(1.25, 0.38, 0.145, 0.151, 0.9),
                       y = c(0.95, 0.71, 0.35, 0.14, 0.135))
    Kiruna <- tibble(x = c(0.9, 0.7, 0.12, 0.151),
                     y = c(0.135, 0.035, 0.091, 0.14))
    IOCG <- tibble(x = c(0.145, 0.151, 0.12, 0.047, 0.047, 0.085),
                   y = c(0.35, 0.14, 0.091, 0.035, 0.215, 0.4))
    BIF <- tibble(x = c(0.047, 0.025, 0.0105, 0.047),
                  y = c(0.035, 0.022, 0.12, 0.12))
    FeTiV <- tibble(x = c(1.5, 20, 20, 0.55, 0.9, 1.25),
                    y = c(3, 3, 0.01, 0.01, 0.135, 0.95))

    mgt_class_template <-
        geom_polygon(data = Skarn, aes(x, y), fill = "green",
                     alpha = 0.1, color = "black") +
        geom_polygon(data = Porphyry, aes(x, y), fill = "blue",
                     alpha = 0.1, color= "black") +geom_polygon(data = Kiruna, aes(x, y), fill = "yellow",
                                                                alpha = 0.1, color = "black") +
        geom_polygon(data = IOCG, aes(x, y), fill = "orange",
                     alpha = 0.1, color = "black") +
        geom_polygon(data = BIF, aes(x, y), fill = "red",
                     alpha = 0.1, color = "black") +
        geom_polygon(data = FeTiV, aes(x, y), fill = "sienna",
                     alpha = 0.1, color = "black") +
        annotate("text", x = 0.01, y = 2, label = "Skarn") +
        annotate("text", x = 5, y = 2, label = "Fe-Ti, V") +
        annotate("text", x = 0.02, y = 0.1, label = "BIF") +
        annotate("text", x = 0.1, y = 0.3, label = "IOCG") +
        annotate("text", x = 0.55, y = 0.2, label = "Porphyry") +
        annotate("text", x = 0.55, y = 0.12, label = "Kiruna") +
        scale_x_log10() +
        scale_y_log10() +
        labs(x = "Ti + V (wt%)", y = "Al + Mn (wt%)")

    return(mgt_class_template)
}
