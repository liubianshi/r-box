box::use(ggplot2[ggsave])
box::use(utils/file)

#' @export
gg <- function(...) {
  args <- list(...)
  if (is.null(args$bg)) args$bg <- "white"
  fn <- do.call(ggsave, args)
  if (Sys.getenv("SSH_TTY") == "") {
    system2(Sys.getenv('OPENER'), fn, wait = FALSE)
  } else {
    file$push(fn, force = TRUE)
  }
  return(fn)
}

