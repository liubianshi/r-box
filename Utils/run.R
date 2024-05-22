#' @export
unless_file_exists <- function(script, path = NULL, ...) {
    stopifnot(length(script) == 1L)
    stopifnot(file.exists(script))
    if (is.null(path)) {
        path <- sub("\\.[Rr]$", ".Rds", script, perl = TRUE)
    }

    l <- list(...)
    l$echo <- lbs::ifthen(l$echo, FALSE)
    l$file <- script

    if (!file.exists(path)) {
        do.call(source, l)
    }
}

