#' squeeze vector
#' c("a", "b", "b", "c", "c", "c") =>
# " c(a = 1, b = 2, c = 3)
#' c("a", "b", NA, "c", "c", "c") =>
# " c(a = 1, b = 1, NA = 1, c = 3)
#' @export
squeeze <- function(x) {
  stopifnot(is.vector(x))
  adj_x <- x[-length(x)]
  diff_position <- c(1, which(is.na(x[-1]) | is.na(adj_x) | x[-1] != adj_x) + 1)
  y <- diff(c(diff_position, length(x) + 1))
  names(y) <- x[diff_position]
  y
}

#' @export
unsqueeze <- function(x) {
  stopifnot(is.numeric(x) && !is.null(names(x)))
  purrr::map2(names(x), x, ~ rep.int(.x, .y)) |> unlist()
}

#' @export
shift <- function(x, drop = TRUE) {
  if (length(x) == 0) {
    return(NULL)
  }
  outer_x <- as.character(substitute(x))
  shiftret <- if (isTRUE(drop) && is.list(x)) {
    x[[1]]
  } else {
    x[1, drop = drop]
  }
  assign(as.character(substitute(x)), x[-1], parent.frame())
  shiftret
}

#' @export
pop <- function(x, drop = TRUE) {
  if (length(x) == 0) {
    return(NULL)
  }
  outer_x <- as.character(substitute(x))
  popret <- if (isTRUE(drop) && is.list(x)) {
    x[[length(x)]]
  } else {
    x[length(x), drop = drop]
  }
  assign(outer_x, x[-length(x)], parent.frame())
  popret
}

#' @export
push <- function(x, ...) {
  values <- list(...)
  stopifnot(length(values) > 0L)
  stopifnot(is.symbol(substitute(x)))
  outer_x <- as.character(substitute(x))
  if (is.null(names(x)))
    names(x) <- rep.int(" ", length(x))
  if (is.null(names(values)))
    names(values) <- rep.int(" ", length(values))

  if (is.list(x)) {
    x <- c(x, values)
  } else {
    x <- do.call(c, c(list(x), values))
  }

  names(x) <- ifelse(is.na(names(x)) | names(x) == "", " ", names(x))
  if (all(names(x) == " ")) {
    names(x) <- NULL
  }

  assign(outer_x, x, parent.frame())
  invisible(get(outer_x, parent.frame()))
}

#' @export
unshift <- function(x, ...) {
  values <- list(...)
  stopifnot(length(values) > 0L)
  stopifnot(is.symbol(substitute(x)))
  outer_x <- as.character(substitute(x))
  if (is.null(names(x)))
    names(x) <- rep.int(" ", length(x))
  if (is.null(names(values)))
    names(values) <- rep.int(" ", length(values))

  if (is.list(x)) {
    x <- c(values, x)
  } else {
    x <- do.call(c, c(values, list(x)))
  }

  names(x) <- ifelse(is.na(names(x)) | names(x) == "", " ", names(x))
  if (all(names(x) == " ")) {
    names(x) <- NULL
  }

  assign(outer_x, x, parent.frame())
  invisible(get(outer_x, parent.frame()))
}

