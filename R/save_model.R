#' save_modal
#'
#' @description
#' simple way to save models for future use
save_model <- function(x) {
    # create model folder if it doesn't exist
    if (!fs::dir_exists("Models")) {
        fs::dir_create("Models")
    }

    # name + file
    name <- deparse(substitute(x))
    file <- fs::path("Models", name, ext = "rds")

    # save
    saveRDS(x, file = file)

    # return
    return(invisible(NULL))
}
