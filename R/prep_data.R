#' prep_data
#'
#' @description
#' WGFD specific data prep/cleaning
prep_data <- function() {
    # pull data
    x <- db_sage_query("./SQL/obs.SQL")

    # time cleaning ----

    # initial cleaning
    x <- x |>
        _[, Time := stringr::str_remove_all(Time, "[:alpha:]")] |>
        _[, Time := trimws(Time)] |>
        _[
            stringr::str_starts(Time, "[:punct:]|[:symbol:]"),
            Time := stringr::str_remove(Time, "[:punct:]|[:symbol:]")
        ] |>
        _[
            stringr::str_ends(Time, "[:punct:]|[:symbol:]"),
            Time := stringi::stri_replace_last_regex(Time, "[:punct:]|[:symbol:]", "")
        ] |>
        _[Time != ""]

    # run function
    x <- x |>
        _[, Time := try_parse_time(Time)]

    # additional cleaning
    x[
        ,
        Time := dplyr::case_when(
            Time == "07:60" ~ "08:00",
            Time == "064:45" ~ "06:44",
            Time == "06:80" ~ "07:20",
            Time == "04:65" ~ "05:05",
            Time == "1239-1245" ~ "12:39",
            Time == "0855-0915" ~ "08:55",
            Time == "0934-0950" ~ "09:34",
            Time == "0845-0905" ~ "08:45",
            Time == "05:64" ~ "06:04",
            Time == "03:68" ~ "04:08",
            Time == "30:06" ~ "03:06",
            Time == "606:38" ~ "06:38",
            Time == "106:49" ~ "06:49",
            Time == "1204-1210" ~ "12:04",
            Time == "1224-1234" ~ "12:24",
            Time == "77:25" ~ "07:25",
            Time == "05:557" ~ "05:57",
            Time == "1215-1231" ~ "12:15",
            Time == "62:33" ~ "06:23",
            Time == "0704-0730" ~ "07:04",
            Time == "63:60" ~ "06:36",
            Time == "03:65" ~ "04:05",
            Time == "06:458" ~ "06:45",
            Time == "06:95" ~ "07:35",
            Time == "0:640" ~ "06:40",
            Time == "60:00" ~ "06:00",
            Time == "01:75" ~ "02:05",
            Time == "71:10" ~ "07:11",
            Time == "63:33" ~ "06:33",
            Time == "70:15" ~ "07:15",
            Time == "06:60" ~ "07:00",
            Time == "70:30" ~ "07:30",
            Time == "06:99" ~ "06:00",
            Time == "06:61" ~ "07:01",
            Time == "08:82" ~ "09:22",
            Time == "00:63" ~ "01:03",
            Time == "70:00" ~ "07:00",
            Time == "54:25" ~ "05:42",
            Time == "54:54" ~ "05:45",
            Time == "061:10" ~ "06:11",
            Time == "50:40" ~ "05:40",
            Time == "0-550" ~ "05:50",
            .default = Time
        )
    ]

    # final parsing
    x <- x |>
        _[, YMDHMS := paste0(Date, " ", Time)] |>
        _[, Parsed := lubridate::ymd_hm(YMDHMS)] |>
        _[!is.na(Parsed)] |>
        suppressWarnings() |>
        dplyr::select(-YMDHMS)

    # name
    data.table::setnames(x, "Parsed", "DateTime")

    # geo ----

    # cast to sf
    x[, crs := paste0(269, Zone)]
    x <- utm_to_latlon(
        x,
        xcol = "UTMX",
        ycol = "UTMY",
        zonecol = "Zone"
    )

    # get moon illumination
    # ?suncalc::getMoonIllumination()
    # this doesn't really change by location
    x <- moon_illumination(x, date_col = "DateTime")

    # order
    x <- x |>
        dplyr::arrange(LekID, Date) |>
        sf::st_drop_geometry() |>
        dplyr::select(LekID, LekMGMT, DateTime, Males, MoonFraction) |>
        data.table::as.data.table()

    # filter
    x <- x[lubridate::year(DateTime) >= 2000 & !is.na(Males)]

    # time for models
    x <- x |>
        dplyr::mutate(
            Year = lubridate::year(DateTime),
            Month = lubridate::month(DateTime),
            Day = lubridate::day(DateTime),
            YDay = lubridate::yday(DateTime),
            WOY = lubridate::week(DateTime),
            Time = lubridate::round_date(DateTime, unit = "15 minutes")
        ) |>
        dplyr::mutate(
            Time = DateTimeFraction(Time)
        )

    # mgmt = factor
    x[, LekMGMT := as.factor(LekMGMT)]

    # reasonable times only
    x <- x[data.table::between(Time, 4, 10)]

    # return
    return(x)
}
