# function to try to parse dates
try_parse_time <- function(x) {
    fun <- function(x) {
        t <- lubridate::hm(x) |> suppressWarnings()
        if (!is.na(t)) {
            return(x)
        }
        if (x == "07:?") x <- "0700"
        l <- x |> stringr::str_length()
        if (l == 3) x <- paste0(0, x)
        l <- x |> stringr::str_length()
        if (l != 4) {
            if (l == 2) {
                x <- paste0(x, "00")
            }
            if (l == 5) {
                x <- stringr::str_remove(x, "[:digit:]")
            }
        }
        if (x == "0") x <- "0000"
        if (x == "3") x <- "0300"
        x <- strsplit(x, "") |> unlist()
        x1 <- x[1:2] |> paste0(collapse = "")
        x2 <- x[3:4] |> paste0(collapse = "")
        paste0(c(x1, x2), collapse = ":")
    }
    lapply(x, fun) |> unlist()
}

DateTimeFraction <- function(x) {
    if (!inherits(x, "POSIXct")) {
        stop("x is not a POSIXct object", call. = FALSE)
    }
    hr <- lubridate::hour(x)
    min <- lubridate::minute(x) / 60
    result <- hr + min
    return(result)
}
