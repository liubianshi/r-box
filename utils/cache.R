box::use(fs)

#' Cache normal result
#' @export
result <- function(expressions, filename, update = FALSE) {
  if (grepl("\\.rds$", filename, ignore.case = TRUE)) {
    readfile <- readRDS
    writefile <- saveRDS
  } else {
    box::use(qs)
    readfile <- qs$qread
    writefile <- qs$qsave
  }

  path <- if (fs::path_dir(filename) %in% c(".", getwd())) {
    file.path("cache", filename)
  } else {
    filename
  }
  if (fs::is_link(path)) {
    path <- fs::link_path(path)
  }

  if (file.exists(path) && isFALSE(update)) {
      return(do.call(readfile, list(path)))
  }

  expressions <- substitute(expressions)
  env <- new.env(parent = parent.frame(n = 1))
  res <- eval(expressions, envir = env)
  do.call(writefile, list(res, path))

  return(res)
}


#' Cache regression table
#' @export
estout <- function(sourcefile, outfile = NULL, update = FALSE) {
  if (is.null(outfile)) {
    outfile <- gettextf("table_%s", sub("\\.[Rr]$", ".qs", basename(sourcefile)))
  }

  if (grepl("\\.rds$", outfile, ignore.case = TRUE)) {
    readfile <- readRDS
    writefile <- saveRDS
  } else {
    box::use(qs)
    readfile <- qs$qread
    writefile <- qs$qsave
  }

  path <- file.path("cache", "out", outfile)
  if (file.exists(path) && isFALSE(update)) {
      return(do.call(readfile, list(path)))
  }
  res <- source(sourcefile, local = TRUE)[['value']]
  do.call(writefile, list(res, path))
  return(res)
}


object_cachem <- function(name, expressions, cache, update) {
    box::use(cachem[cache_mem, is.key_missing])
    if (isFALSE(update)) {
      res <- cache$get(name)
      if (!is.key_missing(res)) return(res)
    }

    expressions <- substitute(expressions)
    env <- new.env(parent = parent.frame(n = 1))
    res <- eval(expressions, envir = env)
    cache$set(name, res)
    return(res)
}

object_environment <- function(name, expressions, env, update) {
  stopifnot(is.environment(env))
  if (utils::hasName(env, name) && isFALSE(update)) {
    return(get(name, env))
  }

  expressions <- substitute(expressions)
  env <- new.env(parent = parent.frame(n = 1))
  res <- eval(expressions, envir = env)
  assign(name, res, envir = env)
  return(res)
}


#' @export
object <- function(name, expressions, cache = parent.frame(), update = FALSE) {
  if (inherits(cache, "cachem")) {
    invisible(object_cachem(name, expressions, cache, update))
  } else {
    invisible(object_environment(name, expressions, cache, update))
  }
}


