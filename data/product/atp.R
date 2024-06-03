box::use(magrittr[`%>%`, `%<>%`, `%T>%`])
box::use(stringr[glue = str_glue])
box::use(data.table[setDT, as.data.table, data.table, setnames, fread])


#' @export
usa_import <- function(year, hts = FALSE) {
  data <-
    gettextf("https://www.census.gov/foreign-trade/reference/codes/atp/impatp%d.txt", year %% 100) %>%
    fread(sep = "\003", head = FALSE)
  data[, `:=`(
    atp = stringr::str_sub(V1, 1, 2),
    hts = stringr::str_sub(V1, 4, 13),
    desc = stringr::str_sub(V1, 15)
  )]
}

