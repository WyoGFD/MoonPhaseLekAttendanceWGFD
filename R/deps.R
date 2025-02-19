#' deps
#' 
#' @description
#' Install repo dependencies
deps <- function() {
    # get installed packages
    inst <- utils::installed.packages() |>
        as.data.frame() |>
        getElement("Package")

    # check for renv
    if (!"renv" %in% inst) {
        utils::install.packages("renv", repos = "https://cloud.r-project.org")
    }

    # get dir dependencies
    pkgs <- renv::dependencies(quiet = TRUE) |>
        getElement("Package") |>
        unique() |>
        sort()

    # get missing & install
    if (any(!pkgs %in% inst)) {
        to_install <- pkgs[!pkgs %in% inst]
        renv::install(to_install, prompt = FALSE)
    }

    return(invisible(NULL))
}