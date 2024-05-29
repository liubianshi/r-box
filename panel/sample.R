box::use(purrr[reduce])
box::use(data.table[as.data.table])
box::use(stringr[str_split_fixed])
box::use(stats[na.omit])

#' @export
cross <- function(...)
{
    l <- list(...)
    if (length(l) < 2L) stop("Need at list two vector", call. = FALSE)
    out <- reduce(l, ~ {
        .x <- .x[!is.na(.x)]
        .y <- .y[!is.na(.y)]
        V1 <- rep(.x, each  = length(.y))
        V2 <- rep(.y, times = length(.x))
        paste(V1, V2, sep = "\003")
    })

    out <-
        str_split_fixed(out, "\003", length(l)) |>
        as.data.table()

    if (!is.null(names(l))) {
        names(out) <- ifelse(names(l) == "", names(out), names(l))
    }

    return(out)
}

cross(1:3, 2:4)
