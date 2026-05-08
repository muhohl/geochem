#' Magnetite discrimination diagram after Dupuis & Beaudoin (2011)
#'
#' Returns a list of ggplot2 layers that draw the Ti+V vs Al+Mn magnetite
#' discrimination fields. Add the result directly to a [ggplot2::ggplot()] call
#' alongside [ggplot2::geom_point()]. The x-axis maps to **Ti + V (wt %)** and
#' the y-axis to **Al + Mn (wt %)**, both on log~10~ scales.
#'
#' Six deposit-type fields are drawn: Skarn, Fe-Ti & V, Porphyry, Kiruna,
#' IOCG, and BIF.
#'
#' @param labels Logical; if `TRUE` (default) field name annotations are drawn.
#' @param fields Logical; if `TRUE` (default) field boundary lines are drawn.
#' @param color_fill Logical; if `TRUE` (default) fields are filled with
#'   translucent colours.
#' @param color Colour used for field boundary lines and label text
#'   (default `"black"`).
#' @param textsize Font size for the field name annotations (default `3`).
#' @param axis_labels Logical; if `TRUE` (default) axis labels are added. Set to `FALSE` when layering multiple diagram calls to
#'   avoid duplicate-scale warnings.
#'
#' @return A list of ggplot2 layer objects.
#' @export
#'
#' @references Dupuis, C., & Beaudoin, G. (2011). Discriminant diagrams for
#'   iron oxide trace element fingerprinting of mineral deposit types.
#'   *Mineralium Deposita*, 46(4), 319–335.
#'   \doi{10.1007/s00126-011-0334-y}
#'
#' @examples
#' # Overlay on scatter plot of magnetite data
#' # (columns Ti_V = Ti + V wt%, Al_Mn = Al + Mn wt%)
#' \dontrun{
#' ggplot2::ggplot(magnetite_data, ggplot2::aes(x = Ti_V, y = Al_Mn)) +
#'     geom_magnetite_deposit() +
#'     ggplot2::geom_point()
#' }
#'
#' # Template only (no data)
#' ggplot2::ggplot() + geom_magnetite_deposit()
geom_magnetite_deposit <- function(
    labels = TRUE,
    fields = TRUE,
    color_fill = TRUE,
    color = "black",
    textsize = 3,
    axis_labels = TRUE
) {
    Skarn <- tibble::tibble(
        x = c(0.005, 1.5, 1.25, 0.38, 0.145, 0.085, 0.047, 0.047, 0.005),
        y = c(3, 3, 0.95, 0.71, 0.35, 0.4, 0.215, 0.12, 0.12)
    )
    Porphyry <- tibble::tibble(
        x = c(1.25, 0.38, 0.145, 0.151, 0.9),
        y = c(0.95, 0.71, 0.35, 0.14, 0.135)
    )
    Kiruna <- tibble::tibble(
        x = c(0.9, 0.7, 0.12, 0.151),
        y = c(0.135, 0.035, 0.091, 0.14)
    )
    IOCG <- tibble::tibble(
        x = c(0.145, 0.151, 0.12, 0.047, 0.047, 0.085),
        y = c(0.35, 0.14, 0.091, 0.035, 0.215, 0.4)
    )
    BIF <- tibble::tibble(
        x = c(0.047, 0.025, 0.0105, 0.047),
        y = c(0.035, 0.022, 0.12, 0.12)
    )
    FeTiV <- tibble::tibble(
        x = c(1.5, 20, 20, 0.55, 0.9, 1.25),
        y = c(3, 3, 0.01, 0.01, 0.135, 0.95)
    )

    field_data <- list(
        list(data = Skarn, fill = "green"),
        list(data = Porphyry, fill = "blue"),
        list(data = Kiruna, fill = "yellow"),
        list(data = IOCG, fill = "orange"),
        list(data = BIF, fill = "red"),
        list(data = FeTiV, fill = "sienna")
    )

    fill_col <- if (color_fill) NULL else NA # NULL = use per-field colour
    border_col <- if (fields) color else NA

    layers <- lapply(field_data, function(fd) {
        ggplot2::geom_polygon(
            data = fd$data,
            ggplot2::aes(x, y),
            fill = if (color_fill) fd$fill else NA,
            alpha = if (color_fill) 0.1 else 0,
            color = border_col,
            inherit.aes = FALSE
        )
    })

    if (labels) {
        label_df <- tibble::tibble(
            x = c(0.01, 5, 0.02, 0.1, 0.55, 0.55),
            y = c(2, 2, 0.1, 0.3, 0.2, 0.12),
            label = c("Skarn", "Fe-Ti,V", "BIF", "IOCG", "Porphyry", "Kiruna")
        )
        layers <- c(
            layers,
            list(
                ggplot2::geom_text(
                    data = label_df,
                    ggplot2::aes(x = x, y = y, label = label),
                    colour = color,
                    size = textsize,
                    inherit.aes = FALSE
                )
            )
        )
    }

    if (axis_labels) {
        layers <- c(
            layers,
            list(
                ggplot2::labs(x = "Ti + V (wt%)", y = "Al + Mn (wt%)")
            )
        )
    }

    layers
}
