box::use(magrittr[`%>%`, `%<>%`, `%T>%`])
box::use(stringr[glue = str_glue, str_match_all, str_detect])
box::use(data.table[setDT, as.data.table, data.table, setnames])
box::use(purrr)
box::use(jsonlite[fromJSON])
box::use(httr)

CACHE_PATH <- file.path(Sys.getenv("NUTSTORE"), "Data", "CACHE", "USA_HTS")
dir.create(CACHE_PATH, showWarnings = FALSE, recursive = TRUE, mode = "0777")
BASE_URL <- "https://www.usitc.gov/sites/default/files/tata/hts"

gen_filename <- function(year, vesion) {
  gettextf(
    'hts_%s_revision_%s_json.json',
    year,
    ifelse(version == 0, "basicb", version)
  )
}

fetch_json <- function(filename) {
  response <- httr$GET(gettextf("%s/%s", BASE_URL, filename))
  if (httr$status_code(response) == 200) {
    writeBin(httr$content(response, "raw"), file.path(CACHE_PATH, filename))
    cat("Fetch Success:", filename, "\n")
  } else {
    stop("Fetch fail!\n", call. = FALSE)
  }
}

read_data <- function(filename) {
  filepath <- file.path(CACHE_PATH, filename)
  if (!file.exists(filepath)) {
    fetch_json(filename)
  }
  return(fromJSON(filepath, simplifyVector = FALSE))
}

parse_item_china_special_tariff <- function(footnotes) {
  single_footnote <- function(fnt) {
    if (is.null(fnt) || length(fnt$columns) == 0) return("")
    if (length(fnt$columns) == 1 && fnt$columns[[1]] == "other") return("")
    str_match_all(fnt$value, "9903\\.88\\.[0-9]{2}") %>%
    purrr::map(~ .x[, 1]) %>%
    purrr::flatten_chr()
  }

  purrr::map(footnotes, single_footnote) %>%
  purrr::flatten_chr() %>%
  paste0(collapse = "\t")
}

parse_item <- function(item) {
  dt <- data.table(
    htsno = item$htsno,
    general = item$general,
    special = item$special,
    other = item$other,
    units =
      if (is.list(item$units)) {
        paste0(purrr::flatten_chr(item$units), collapse = "\t")
      } else {
        item$units
      },
    china_tariff = ""
  )
  if (str_detect(item$htsno, "^[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}")) {
    dt$china_tariff <- parse_item_china_special_tariff(item$footnotes)
  }
  dt
}

filename <- gen_filename(2021, 0)
jq <- read_data(filename)


re <- purrr::map_dfr(jq, parse_item)
re[, .N, by = china_tariff][, .(china_tariff, sort(N, TRUE))] %>% head(20)


