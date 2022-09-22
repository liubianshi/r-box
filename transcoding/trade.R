#' @export
conversion <- function(from, to)
{
    names_replace <- function(x, ...) {
        l <- list(...)
        purrr::walk2(names(l), l, ~ {
            x[stringr::str_starts(x, tolower(.y))] <- .x
        })
        x
    }

    data <- lbs::getDataSQLite("Coding_Inds", gettextf("%s_%s", from, to))
    data <- data[, .SD, .SDcols = patterns("code$")]

    data.table::setnames(data, names_replace(tolower(names(data)), from = from, to = to))
    data[, .(from = as.character(from), to = as.character(to))]
    data.table::setnames(data, c(from, to))

    data
}


