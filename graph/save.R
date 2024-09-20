box::use(ggplot2[ggsave])
box::use(utils/file)

#' @export
gg <- function(...) {
  args <- list(...)
  fn <- do.call(ggsave, args)
  file$push(file, force = TRUE)
  return(fn)
}

