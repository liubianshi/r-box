box::use(magrittr[`%>%`, `%<>%`, `%T>%`])
box::use(stringr[glue = str_glue])
box::use(data.table[setDT, as.data.table, data.table, setnames, fread])


#' @export
usa_import <- function(year, hts = FALSE) {
  data <- fread(
    file = gettextf("./data/product/raw/impatp%d.txt", year %% 100),
    sep = "\003",
    head = FALSE
  )
  data[, `:=`(
    atp = stringr::str_sub(V1, 1, 2),
    hts = stringr::str_sub(V1, 4, 13),
    desc = stringr::str_sub(V1, 15)
  )][, V1 := NULL]

  usa_import <- fread("./data/product/raw/usa_import_hts_10bit_2017-2023.csv")
  usa_import[, V5 := NULL][, Country := NULL]
  usa_import[, hts := stringr::str_sub(Commodity, 1, 10)]
  import <- usa_import[, .(hts, Time, value = `Customs Value (Gen) ($US)`)][Time == year]

  import <- merge(import, data, all.x = TRUE, all.y = FALSE, by = "hts")
  import[is.na(atp), atp := "00"]
  import[, hs6 := substr(hts, 1, 6)]
  import[atp != "00", .(atp = length(unique(atp))), by = hs6][, .N, by = atp]

  usa_hts_2019 <- fread(file = "./data/product/raw/hts_2019_rev_20_data_csv.csv")
  usa_hts_2019 %<>% .[!lbs::isempty(`HTS Number`), .(hts = `HTS Number`, indent = Indent, desc = Description)]
  usa_hts_2019[, hts := stringr::str_replace_all(hts, '\\.', "")]
}

