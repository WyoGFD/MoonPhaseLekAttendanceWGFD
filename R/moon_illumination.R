#' moon_illumination
#'
#' @description simple wrapper for suncalc::getMoonIllumination()
moon_illumination <- function(x, date_col = NULL) {
    # try to guess date col
    if (is.null(date_col)) {
        # find date col
        date_col <- x |>
            data.table::data.table() |>
            _[, lapply(.SD, function(x) inherits(x, c("Date", "POSIXct")))]
        date_col <- names(x)[date_col |> unlist()]

        if (length(date_col) < 1) {
            stop("Cannot find Date or POSIXct column. Specify with 'date_col' = ?", call. = FALSE)
        }

        if (length(date_col) > 1) {
            date_col <- paste0(date_col, collapse = ", ")
            stop("\nMultiple columns are of class Date or POSIXct: ", date_col, ".\nSpecify with 'date_col' = ?")
        }
    }

    # get date
    date <- x |>
        data.table::as.data.table() |>
        _[, as.Date(get(date_col))]

    # get illumination
    moon <- date |>
        suncalc::getMoonIllumination() |>
        data.table::as.data.table() |>
        _[, date := NULL]
    moon_names <- paste0("Moon", stringr::str_to_title(colnames(moon)))
    data.table::setnames(moon, moon_names)

    # bind
    x <- cbind(x, moon)

    # return
    return(x)
}
