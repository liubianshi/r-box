#' @export
Result <- function(expressions, cache, update = FALSE) {
    expressions <- substitute(expressions)
    path <- file.path(".cache", cache)
    if (file.exists(path) && isFALSE(update)) {
        return(readRDS(path))
    }
    res <- eval.parent(expressions, n = 1)
    saveRDS(res, path, compress = FALSE)
    return(res)
}
