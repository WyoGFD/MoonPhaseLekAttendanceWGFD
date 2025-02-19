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

    # install cmdstanr
    if (!"cmdstanr" %in% inst) {
        utils::install.packages(
            "cmdstanr",
            repos = c(
                "https://stan-dev.r-universe.dev",
                getOption("repos")
            )
        )
        cmdstanr::install_cmdstan()
    }

    # get installed packages
    inst <- utils::installed.packages() |>
        as.data.frame() |>
        getElement("Package")

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
