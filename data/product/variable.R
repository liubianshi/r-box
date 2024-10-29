box::use(magrittr[`%>%`, `%<>%`, `%T>%`])
box::use(stringr[glue = str_glue])
box::use(data.table[setDT, as.data.table, data.table, setnames])
box::use(lbs)

cal_export_share <- function(hsdata, country_code, special, other) {
  stopifnot(all(
    sort(names(hsdata)) == sort(c("hs", "country_code", special, other))
  ))
  setnames(hsdata, c("country_code", special, other), c("country", "special", "other"))

  share_all <- hsdata %>%
    .[, lapply(.SD, sum, na.rm = TRUE), by = hs, .SDcols = c("other", "special")] %>%
    .[, .(hs, share_all = special / (special + other))]

  share_country <- hsdata %>%
    .[ country == country_code ] %>%
    .[, lapply(.SD, sum, na.rm = TRUE), by = hs, .SDcols = c("other", "special")] %>%
    .[, .(hs, share_country = special / (other + special))]

  share <- hsdata %>%
    .[, country_special := ifelse(country == ..country_code, special, 0)] %>%
    .[, country := NULL] %>%
    .[, lapply(.SD, sum, na.rm = TRUE), by = hs ] %>%
    .[, .(hs, share = country_special / (other + special))]

  re <-
    data.table(hs = unique(c(share_all$hs, share_country$hs, share$hs))) %>%
    share_all[., on = "hs"] %>%
    share_country[., on = "hs"] %>%
    share[., on = "hs"] %>%
    setnames(c("hs", paste(special, c("share", "share_country", "share_all"), sep = "_")))
  re
}

#' @export
hs_export_processing_share <- function(year, country_code = NULL, country_name = NULL) {
  stopifnot(!is.null(country_code) || !is.null(country_name))

  hsdata <- lbs::getDataSQLite("CHN_FirmTrade", glue("HG{year}"), condition = "IM_EX = 'EX'")
  hsdata[, IM_EX := NULL]

  if (is.null(country_code)) {
    country_code <- hsdata[country == country_name, country_code[1]]
    stopifnot(!lbs::isempty(country_code))
  }
  hsdata[, processing := ifelse(stringr::str_detect(tradetype, "^\\s*加工贸易"), "processing", "other")]
  hsdata[, hs := stringr::str_sub(HScode, 1, 6)]
  hsdata <- hsdata %>%
    .[, .(export = sum(value, na.rm = TRUE)), by = .(processing, country_code, hs) ] %>%
    data.table::dcast(hs + country_code ~ processing, value.var = "export")
  cal_export_share(hsdata, country_code, "processing", "other")
}


#' @export
hs_export_foreign_share <- function(year, country_code = NULL, country_name = NULL) {
  stopifnot(!is.null(country_code) || !is.null(country_name))
  hsdata <- lbs::getDataSQLite("CHN_FirmTrade", glue("HG{year}"), condition = "IM_EX = 'EX'")
  hsdata[, IM_EX := NULL]
  hsdata[, foreign := ifelse(stringr::str_sub(firmcodeHG, 6, 6) %in% c("2", "3", "4"), "foreign", "domestic")]

  if (is.null(country_code)) {
    country_code <- hsdata[country == country_name, country_code[1]]
    stopifnot(!lbs::isempty(country_code))
  }

  hsdata[, hs := stringr::str_sub(HScode, 1, 6)]
  hsdata <- hsdata[, .(export = sum(value, na.rm = TRUE)), by = .(foreign, country_code, hs) ]
  hsdata <- data.table::dcast(hsdata, hs + country_code ~ foreign, value.var = "export")

  cal_export_share(hsdata, country_code, "foreign", "domestic")
}


