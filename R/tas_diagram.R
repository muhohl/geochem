#' Total Alkali Silica diagram
#'
#' Template for the Total Alkali Silica (TAS) diagram after Le Maitre (1989).
#' The function returns a list of ggplot2 layers that can be added directly to
#' a [ggplot2::ggplot()] call. Map `SiO2` (wt %) to x and `Na2O + K2O` (wt %)
#' to y; colour or shape aesthetics should be placed inside [ggplot2::geom_point()]
#' rather than the global [ggplot2::aes()] so they are not inherited by the
#' diagram lines.
#'
#' Field labels are drawn with [ggfittext::geom_fit_text()]: the font size is
#' automatically reduced so the text fits within each field's bounding box.
#'
#' @param fields If `TRUE` (default), draw the field boundary lines.
#' @param labels If `TRUE` (default), draw field name labels.
#'
#' @return A list of ggplot2 layer objects.
#' @export
#'
#' @examples
#' ggplot2::ggplot() +
#'     geochem::geom_tas_diagram()
geom_tas_diagram <- function(fields = TRUE, labels = TRUE) {
    tas <- tibble::tribble(
        ~label , ~x     , ~y   ,
        "p1"   , 41     ,  1   ,
        "p2"   , 45     ,  1   ,
        "p3"   , 41     ,  7   ,
        "p4"   , 45     ,  5   ,
        "p5"   , 52     ,  5   ,
        "p6"   , 57     ,  5.9 ,
        "p7"   , 63     ,  7   ,
        "p8"   , 69     ,  8   ,
        "p9"   , 69     , 13   ,
        "p10"  , 61     , 13.5 ,
        "p11"  , 57.6   , 11.7 ,
        "p12"  , 53     ,  9.3 ,
        "p13"  , 49.4   ,  7.3 ,
        "p14"  , 45     ,  9.4 ,
        "p15"  , 48.4   , 11.5 ,
        "p16"  , 52.5   , 14   ,
        "p17"  , 78     ,  1   ,
        "p18"  , 48.065 , 16   ,
        "p19"  , 63     ,  1   ,
        "p20"  , 57     ,  1   ,
        "p21"  , 52     ,  1
    )

    # ── boundary lines ────────────────────────────────────────────────────────
    l1 <- tas |> dplyr::filter(label %in% c("p1", "p3", "p14", "p16"))
    l2 <- tas |> dplyr::filter(label %in% c("p2", "p4", "p13", "p10"))
    l3 <- tas |> dplyr::filter(label %in% c("p4", "p5", "p8"))
    l4 <- tas |> dplyr::filter(label %in% c("p18", "p16", "p11", "p7", "p19"))
    l5 <- tas |> dplyr::filter(label %in% c("p15", "p12", "p6", "p20"))
    l6 <- tas |> dplyr::filter(label %in% c("p9", "p8"))
    l7 <- tas |> dplyr::filter(label %in% c("p14", "p13", "p5", "p21"))
    l8 <- tas |> dplyr::filter(label %in% c("p8", "p17"))

    tas_template <- list(
        ggplot2::ylab(expression(paste(Na[2], "O + ", K[2], "O [wt%]"))),
        ggplot2::xlab(expression(paste("Si", O[2], " [wt%]")))
    )

    if (fields) {
        tas_template <- append(
            tas_template,
            list(
                ggplot2::geom_line(
                    data = l1,
                    ggplot2::aes(x, y),
                    inherit.aes = FALSE
                ),
                ggplot2::geom_line(
                    data = l2,
                    ggplot2::aes(x, y),
                    inherit.aes = FALSE
                ),
                ggplot2::geom_line(
                    data = l3,
                    ggplot2::aes(x, y),
                    inherit.aes = FALSE
                ),
                ggplot2::geom_line(
                    data = l4,
                    ggplot2::aes(x, y),
                    inherit.aes = FALSE
                ),
                ggplot2::geom_line(
                    data = l5,
                    ggplot2::aes(x, y),
                    inherit.aes = FALSE
                ),
                ggplot2::geom_line(
                    data = l6,
                    ggplot2::aes(x, y),
                    inherit.aes = FALSE
                ),
                ggplot2::geom_line(
                    data = l7,
                    ggplot2::aes(x, y),
                    inherit.aes = FALSE
                ),
                ggplot2::geom_line(
                    data = l8,
                    ggplot2::aes(x, y),
                    inherit.aes = FALSE
                )
            )
        )
    }

    if (labels) {
        # Bounding boxes are conservative rectangles that lie within each field
        # polygon. ggfittext shrinks the text automatically to fit inside the box.
        #
        # Field polygon vertices (from the line definitions above):
        #   Picrobasalt          p1-p2-p4-p3   (top is diagonal p3→p4)
        #   Basalt               p2-p21-p5-p4  (rectangle)
        #   Basaltic andesite    p21-p20-p6-p5
        #   Andesite             p20-p19-p7-p6
        #   Dacite               p19-p8-p7     (l4 left, l3 top, l8 right)
        #   Rhyolite             p8-p17        (right of l8, above l6)
        #   Tephrite/basanite    p3-p14-p13-p4 (l1 left, l7 upper, l2 right)
        #   Trachy-basalt        p4-p13-p5     (triangle above l3)
        #   Bas. trachy-and.     p13-p5-p6-p12
        #   Trachy-andesite      p12-p6-p7-p11
        #   Trachyte             p11-p7-p8-p9-p10
        #   Phono-tephrite       p14-p13-p12-p15
        #   Tephri-phonolite     p15-p12-p11-p16
        #   Foidite              p3-p14-p16-p18 (above l1)
        #   Phonolite            p16-p11-p10-p18 (between l4 upper and l2 upper)

        field_labels <- tibble::tribble(
            ~label                               , ~xmin , ~xmax , ~ymin , ~ymax ,
            # ── sub-alkaline series (bottom row) ──────────────────────────────
            "Picro-\nbasalt"                     , 41.1  , 44.5  ,  1    ,  4    ,
            "Basalt"                             , 45    , 52    ,  1    ,  5    ,
            "Basaltic\nandesite"                 , 52    , 57    ,  1    ,  5    ,
            "Andesite"                           , 57    , 63    ,  1    ,  5.5  ,
            "Dacite"                             , 63.5  , 68.5  ,  1    ,  6.5  ,
            "Rhyolite"                           , 70    , 78    ,  7.5  , 12    ,
            # ── alkaline / transitional series ────────────────────────────────
            "Tephrite\nOl<10%\nBasanite\nOl>10%" , 40.2  , 49.0  ,  6.0  ,  7.8  ,
            "Trachy-\nbasalt"                    , 46    , 52    ,  5.2  ,  6.1  ,
            "Basaltic\ntrachy-\nandesite"        , 51.1  , 55.5  ,  5.7  ,  7.7  ,
            "Trachy-\nandesite"                  , 54    , 62    ,  6.5  , 11    ,
            "Trachyte"                           , 58.5  , 69    ,  8    , 13    ,
            # ── phonolite series (left alkaline) ──────────────────────────────
            "Phono-\ntephrite"                   , 45.5  , 52.5  ,  8    , 11    ,
            "Tephri-\nphonolite"                 , 49.5  , 57    , 10    , 13.5  ,
            "Foidite"                            , 41    , 51    , 12    , 16    ,
            "Phonolite"                          , 53    , 63    , 12.5  , 15.5
        )

        tas_template <- append(
            tas_template,
            list(
                ggfittext::geom_fit_text(
                    data = field_labels,
                    ggplot2::aes(
                        xmin = xmin,
                        xmax = xmax,
                        ymin = ymin,
                        ymax = ymax,
                        label = label
                    ),
                    inherit.aes = FALSE,
                    reflow = TRUE,
                    grow = FALSE,
                    min.size = 1,
                    colour = "grey20",
                    padding.x = grid::unit(0.5, "mm"),
                    padding.y = grid::unit(0.5, "mm")
                )
            )
        )
    }

    return(tas_template)
}
