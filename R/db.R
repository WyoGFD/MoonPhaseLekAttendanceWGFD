#' db_sage
#'
#' @description connect WGF SAGE database
#'
#' @returns a DBI connection
#'
#' @export
db_sage <- function() {
    # default configuration
    default_config <- c(
        driver = "ODBC Driver 18 for SQL Server",
        server = "WGFSQLREPLNU",
        database = "SAGE",
        Encrypt = "no",
        Trusted_Connection = "yes"
    )

    if (file.exists("config.yml")) {
        # message
        cli::cli_inform(c(i = "{.path config.yml} found. Parsing for connection arguments."))

        # Load configuration
        x <- config::get()

        # Function to check if an entry has all required attributes
        has_required_attributes <- function(entry) {
            required_attributes <- c("driver", "server", "uid", "pwd", "database")
            all(required_attributes %in% names(entry))
        }

        # Filter entries that have the required attributes
        valid_entries <- Filter(has_required_attributes, x)

        # if there are no valid entries, fall back to default
        if (length(valid_entries) == 0) {
            cli::cli_inform(
                c(i = "No entries with required connection attributes found in {.path config.yml}. Using default configuration.") # nolint
            )
            args <- default_config
        }

        # if there are, return the first one
        if (length(valid_entries) >= 1) {
            if (length(valid_entries) > 1) {
                cli::cli_inform(c(i = "Multiple entries with required attributes found. Returning the first match."))
            }

            # message
            cli::cli_inform(
                c(i = "Utilizing {.val {names(valid_entries[1])}} configuration.")
            )

            # Return the first valid entry
            args <- valid_entries[[1]]
        }
    } else {
        args <- default_config
    }

    args <- c(odbc::odbc(), args)

    # connect
    con <- try(do.call(DBI::dbConnect, args), silent = TRUE)

    if (inherits(con, "try-error")) {
        cli::cli_abort("Could not connect to SAGE. Check that you are on the WGF Network.")
    }

    return(con)
}

#' db_sage_query
#'
#' @description \code{DBI::dbGetQuery()} on the SAGE database.
#'
#' @export
db_sage_query <- function(query) {
    # connect
    con <- db_sage()

    # see if it's a file
    if (tools::file_ext(query) %in% c("SQL", "sql")) {
        # and that it exists
        if (fs::file_exists(query)) {
            query <- readr::read_file(query)
        }
    }

    # get query
    x <- DBI::dbGetQuery(con, statement = query) |> data.table::as.data.table()

    # disconnect
    DBI::dbDisconnect(con)

    # return
    return(x)
}
