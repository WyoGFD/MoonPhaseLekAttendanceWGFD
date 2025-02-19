#' theme_wgfd
#'
#' @export
theme_wgfd <- function(font_family = "Garamond", font_size = 12, main_color = "black", bg_color = "transparent") {
    # load fonts
    extrafont::loadfonts("all", quiet = TRUE)

    # update geom_text
    ggplot2::update_geom_defaults("text", list(family = font_family, color = main_color))

    # add transparency for grid
    grid_color <- grDevices::col2rgb(main_color)
    grid_color <- paste0(
        grDevices::rgb(grid_color[1],
            grid_color[2],
            grid_color[3],
            maxColorValue = 255
        ),
        20
    )

    # define theme
    theme <- ggplot2::theme_bw(base_size = font_size) +
        ggplot2::theme(
            axis.line = ggplot2::element_line(
                linewidth = 0.5,
                color = main_color
            ),
            axis.title = ggplot2::element_text(
                face = "bold",
                margin = ggplot2::margin(12, 12, 12, 12, unit = "pt"),
                family = font_family,
                color = main_color
            ),
            axis.text.x = ggplot2::element_text(
                margin = ggplot2::margin(6, 6, 6, 6, unit = "pt"),
                color = main_color,
                family = font_family,
                size = ggplot2::unit(font_size, "points")
            ),
            axis.text.y = ggplot2::element_text(
                margin = ggplot2::margin(6, 6, 6, 6, unit = "pt"),
                color = main_color,
                family = font_family,
                size = ggplot2::unit(font_size, "points")
            ),
            axis.ticks.length = ggplot2::unit(8, "points"),
            axis.ticks = ggplot2::element_line(color = main_color),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid = ggplot2::element_line(color = grid_color),
            panel.background = ggplot2::element_rect(fill = bg_color),
            panel.border = ggplot2::element_blank(),
            plot.background = ggplot2::element_rect(fill = bg_color, color = NA),
            plot.margin = ggplot2::unit(c(5, 5, 5, 5) * 2, "points"),
            plot.title = ggplot2::element_text(
                face = "bold",
                size = ggplot2::unit(font_size + 2, "points"),
                family = font_family,
                margin = ggplot2::margin(6, 0, 6, 0, unit = "pt"),
                color = main_color,
                hjust = 0.5
            ),
            plot.subtitle = ggplot2::element_text(
                face = "italic",
                size = ggplot2::unit(font_size, "points"),
                family = font_family,
                margin = ggplot2::margin(6, 0, 12, 0, unit = "pt"),
                color = main_color,
                hjust = 0.5
            ),
            legend.title = ggplot2::element_text(
                face = "bold",
                family = font_family,
                color = main_color
            ),
            legend.text = ggplot2::element_text(
                family = font_family,
                color = main_color,
                size = ggplot2::unit(font_size, "points")
            ),
            legend.key = ggplot2::element_rect(color = bg_color),
            strip.background = ggplot2::element_blank(),
            strip.text.x = ggplot2::element_text(
                color = main_color,
                size = ggplot2::unit(font_size, "points"),
                margin = ggplot2::margin(10, 0, 10, 0),
                family = font_family,
                face = "bold"
            ),
            strip.text.y = ggplot2::element_text(
                color = main_color,
                size = ggplot2::unit(font_size, "points"),
                angle = -90,
                margin = ggplot2::margin(0, 10, 0, 10),
                family = font_family,
                face = "bold"
            ),
            legend.background = ggplot2::element_rect(fill = bg_color),
            legend.box.background = ggplot2::element_blank()
        )

    # return
    return(theme)
}
