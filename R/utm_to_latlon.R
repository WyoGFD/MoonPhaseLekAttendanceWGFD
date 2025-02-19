#' utm_to_latlon
#'
#' @description takes a multi UTM zone df and casts to lat lon
utm_to_latlon <- function(x, xcol, ycol, zonecol) {
    # to dt, index, get crs
    x <- data.table::as.data.table(x)
    x[, index := .I]

    # empty dt
    coords <- data.table::data.table()
    for (z in unique(x$crs)) {
        y <- x[crs == z] |>
            sf::st_as_sf(
                coords = c(xcol, ycol),
                crs = as.integer(z),
                remove = FALSE
            ) |>
            sf::st_transform(crs = 4326)
        coords <- rbind(coords, y)
    }

    # reorder to original index
    data.table::setorder(coords, index)
    coords[, c("index", "crs") := NULL]
    coords <- sf::st_as_sf(coords)

    # return
    return(coords)
}
