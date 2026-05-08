#' Compute feldspar molar ratios
#'
#' Converts Na, K, and Al columns to moles and appends `na_al_molar` and/or
#' `k_al_molar` ratio columns to `data`. Column names are used to detect
#' whether the input is an oxide (any `"o"` or `"O"` in the column name) or an
#' element weight percent, and the appropriate molar mass is applied
#' automatically.
#'
#' Ratios are only appended when both the numerator element and `al` are
#' supplied. If one is omitted the corresponding column is silently skipped,
#' allowing partial analyses to pass through unmodified.
#'
#' @param data A data frame.
#' @param na Character scalar; name of the Na or Na\ifelse{html}{\out{<sub>2</sub>}}{\eqn{_2}}O column.
#' @param k  Character scalar; name of the K or K\ifelse{html}{\out{<sub>2</sub>}}{\eqn{_2}}O column.
#' @param al Character scalar; name of the Al or Al\ifelse{html}{\out{<sub>2</sub>}}{\eqn{O_3}} column.
#'
#' @return A tibble identical to `data` with up to two additional columns:
#'   `na_al_molar` and `k_al_molar`.
#' @export
#'
#' @examples
#' d <- data.frame(Na2O = 11.8, K2O = 0.2, Al2O3 = 19.4)
#' fsp_molar(d, na = "Na2O", k = "K2O", al = "Al2O3")
#'
#' d2 <- data.frame(Na_pct = 8.77, K_pct = 0.1, Al_pct = 10.29)
#' fsp_molar(d2, na = "Na_pct", k = "K_pct", al = "Al_pct")
fsp_molar <- function(data, na = NULL, k = NULL, al = NULL) {
    to_moles <- function(col, atomic_mass, oxide_mass, n_atoms) {
        if (is.null(col)) return(NULL)
        x <- data[[col]]
        if (grepl("[oO]", col)) x / oxide_mass * n_atoms else x / atomic_mass
    }

    na_mol <- to_moles(na, atomic_mass = 22.99, oxide_mass = 61.98, n_atoms = 2)
    k_mol  <- to_moles(k,  atomic_mass = 39.10, oxide_mass = 94.20, n_atoms = 2)
    al_mol <- to_moles(al, atomic_mass = 26.98, oxide_mass = 101.96, n_atoms = 2)

    if (!is.null(na_mol) && !is.null(al_mol)) data$na_al_molar <- na_mol / al_mol
    if (!is.null(k_mol)  && !is.null(al_mol)) data$k_al_molar  <- k_mol  / al_mol

    tibble::as_tibble(data)
}


#' Feldspar molar ternary plot
#'
#' Creates a ggplot2 canvas for a feldspar molar ratio diagram using the
#' columns produced by [fsp_molar()]. The albite–orthoclase join line
#' (Na/Al + K/Al = 1) is drawn automatically. Add [ggplot2::geom_point()] or
#' other layers on top to display sample data.
#'
#' @param data A data frame with `na_al_molar` and `k_al_molar` columns,
#'   typically the output of [fsp_molar()].
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' data.frame(Na2O = c(11.8, 0.5), K2O = c(0.2, 16.9), Al2O3 = c(19.4, 18.3)) |>
#'   fsp_molar(na = "Na2O", k = "K2O", al = "Al2O3") |>
#'   ggfspmolar() +
#'   ggplot2::geom_point(ggplot2::aes(colour = "sample"), size = 2)
ggfspmolar <- function(data) {
    ggplot2::ggplot(data, ggplot2::aes(x = na_al_molar, y = k_al_molar)) +
        ggplot2::geom_segment(
            ggplot2::aes(x = 0, xend = 1, y = 1, yend = 0),
            inherit.aes = FALSE
        ) +
        ggplot2::scale_x_continuous(limits = c(0, 1), name = "Na/Al (molar)") +
        ggplot2::scale_y_continuous(limits = c(0, 1), name = "K/Al (molar)") +
        ggplot2::coord_fixed()
}
