#' epred
#'
#' @description
#' Wrapper for brms::posterior_epred() for easier use
epred <- function(brmsfit, ndraws = NULL, newdata = NULL) {
    # newdata to dt + get col order
    if (is.null(newdata)) {
        newdata <- insight::get_data(br)
    }
    newdata <- data.table::as.data.table(newdata)
    colorder <- newdata |>
        data.table::copy() |>
        colnames()

    # get predictions
    epred <- brms::posterior_epred(br, ndraws = ndraws, newdata = newdata)
    # add row num for merge
    newdata[, Row := .I]
    colnames(epred) <- seq_len(ncol(epred))
    rownames(epred) <- seq_len(nrow(epred))
    epred <- epred |>
        as.data.frame.table(stringsAsFactors = FALSE) |>
        data.table::as.data.table() |>
        data.table::setnames(c("Draw", "Row", "epred")) |>
        _[, c("Draw", "Row") := lapply(.SD, as.integer), .SDcols = c("Draw", "Row")] |>
        merge(newdata) |>
        _[, Row := NULL] |>
        data.table::setcolorder(neworder = colorder)

    # gc
    gc(full = TRUE)

    return(epred)
}

#' sum_epred
#'
#' @description
#' Take posterior summaries from draws
sum_epred <- function(x) {
    x <- quantile(x, probs = c(0.025, 0.5, 0.975))
    x <- unname(x)
    list(epred.lower = x[1], epred = x[2], epred.upper = x[3])
}

#' conditional_effects
#'
#' @description
#' Wrapper for brms::conditional_effects() to extract data or plots
conditional_effects <- function(brmsfit, effects, plot = TRUE) {
    x <- brms::conditional_effects(brmsfit, effects = effects)
    if (plot) {
        x <- plot(x, plot = FALSE)[[1]]
    } else {
        x <- x[[1]] |> data.table::as.data.table()
        colnames(x) <- snakecase::to_snake_case(colnames(x))
    }
    return(x)
}
