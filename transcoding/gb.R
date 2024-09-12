box::use(magrittr[`%>%`, `%<>%`, `%T>%`])
box::use(stringr[glue = str_glue])
box::use(data.table[setDT, as.data.table, data.table, setnames])
box::use(lbs)
box::use(concordance[concord])

format_code <- function(code) {
  digits <- data.table(digits = nchar(code))
  digits_table <- digits[, .N, by = digits] %>% data.table::setorder(-N)
  stopifnot(digits_table[1, N] == max(digits_table$N, na.rm = TRUE))
  paste0(strrep("0", digits_table$digits[1] - digits$digits), code)
}

get_gbtable <- function(orig, dest) {
  orig <- toupper(orig)
  dest <- toupper(dest)
  re <- lbs::getDataSQLite("Coding_Inds", glue("{orig}_{dest}")) %>%
    .[, .SD, .SDcols = c(glue("{orig}_code"), glue("{dest}_code"))] %>%
    setnames(c("orig", "dest")) %>%
    .[, lapply(.SD, format_code)]
}

concord_direct <- function(sourcevar, table, digit = 4) {
  stopifnot(inherits(table, "data.frame") && length(table) == 2)
  table <- as.data.table(table) %>% setnames(c("orig", "dest"))
  re <- purrr::map(sourcevar, ~ {
    .data <- table[
        stringr::str_sub(orig, 1, nchar(.x)) == .x,
        .(match = stringr::str_sub(dest, 1, digit))
    ]
    if (nrow(.data) == 0) {
      return( list(match = NA, weight = NA) )
    }
    .data[, .(weight = .N), by = match] %>%
    .[, weight := weight / sum(weight) ] %>%
    as.list()
  })
  names(re) <- sourcevar

  nomatch <-
    purrr::imap_chr(re, ~ { if (is.na(.x$match[1])) .y else NA }) %>%
    .[!is.na(.)]
  if (length(nomatch) > 0) {
    warning(
      "Matches for code(s): ",
      paste(nomatch, collapse = ", "),
      " not found and returned NA. Please double check input code and classification.",
      call. = FALSE
    )
  }

  re
}

