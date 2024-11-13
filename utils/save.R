box::use(qs[qsavem, qreadm])
box::use(parallel[detectCores])

# From:
#  Adjust based on https://github.com/sellorm/qsimage/blob/main/R/qsimage.R
#' Saves an image of the global environment
#' @export
qsave_image <- function(file = NULL, nthreads = NULL, env = .GlobalEnv) {
  if (is.null(file))     file <- ".session.qs"
  if (is.null(nthreads)) nthreads <- detectCores() / 2
  do.call(qs::qsavem, c(
    lapply(ls(envir = env), as.symbol),
    file = file,
    nthreads = nthreads
  ))
}

# From:
#  Adjust based on https://github.com/sellorm/qsimage/blob/main/R/qsimage.R
#' Loads an image of the global environment
#' @export
qload_image <- function(file = NULL, nthreads = NULL, env = .GlobalEnv) {
  if (is.null(file))     file <- ".session.qs"
  if (is.null(nthreads)) nthreads <- detectCores() / 2
  qs::qreadm(file, env = env, nthreads = nthreads)
}
