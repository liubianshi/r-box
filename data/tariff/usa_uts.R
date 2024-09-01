box::use(stringr[glue = str_glue, str_match_all, str_detect])
box::use(magrittr[`%>%`, `%<>%`, `%T>%`])
box::use(data.table[setDT, as.data.table, data.table, setnames])
box::use(purrr)
box::use(jsonlite[fromJSON])
box::use(httr[GET, status_code, content])
box::use(rlang[list2])
box::use(rvest[read_html, html_elements, html_attr])

CACHE_PATH <- file.path(Sys.getenv("NUTSTORE"), "Data", "CACHE", "USA_HTS")
dir.create(CACHE_PATH, showWarnings = FALSE, recursive = TRUE, mode = "0777")
BASE_URL <- "https://catalog.data.gov/dataset"
TARIFF_LIST <- c(
  "9903.88.01",
  "9903.88.02",
  "9903.88.03",
  "9903.88.15",
  "9903.88.16"
)

fetch_web <- function(year) {
  url_lib <- gettextf("%s/harmonized-tariff-schedule-of-the-united-states-%s", BASE_URL, year)
  if (year == 2022) {
    url_lib <- gettextf("%s-%s", url_lib, "e2058")
  }
  response <- GET(url_lib)
  if (status_code(response) != 200) {
    stop("Failed to download source code: \n", url_lib)
  }
  content(response, as = "text", encoding = "utf-8")
}

fetch_urls <- function(web) {
  webpage <- read_html(web)
  links <- webpage %>% html_elements(".resource-item")
  info <-  purrr::map_dfr(links, ~ {
    data.table(
      title = html_elements(.x, "a.heading") %>% html_attr("title"),
      href = html_elements(.x, "a.btn.btn-primary") %>% html_attr("href"),
      format = html_elements(.x, "a.btn.btn-primary") %>% html_attr("data-format")
    )
  })
  setDT(info)
  info[format == "json", .(title, href)]
}

fetch_json <- function(title, url) {
  filename <- title %>%
    stringr::str_replace(" \\((\\w+)\\)", ".\\1") %>%
    stringr::str_replace("Rev\\.", "Rev") %>%
    stringr::str_replace_all("\\s", "_") %>%
    tolower()
  filepath <- file.path(CACHE_PATH, filename)
  if (file.exists(filepath)) {
    cat("Exists: ", filename, "\n")
    return()
  }
  response <- httr$GET(url)
  if (httr$status_code(response) == 200) {
    writeBin(httr$content(response, "raw"), filepath)
    cat("Fetch Success:", filename, "\n")
  }
}

update_json_cache <- function(years = 2019:2024) {
  webs <- purrr::map(year, fetch_web)
  urls <- purrr::map_dfr(webs, fetch_urls)
  purrr::walk2(urls$title, urls$href, fetch_json)
}

gen_filename <- function(year, vers) {
  gettextf(
    "%s_hts_%s.json",
    year,
    ifelse(vers == 0, "basic_edition", glue("revision_{vers}"))
  )
}

read_data <- function(year, vers) {
  filepath <- file.path(CACHE_PATH, gen_filename(year, vers))
  if (!file.exists(filepath)) {
    update_json_cache(year)
  }
  return(fromJSON(filepath, simplifyVector = FALSE))
}

parse_item_china_special_tariff <- function(footnotes) {
  single_footnote <- function(fnt) {
    if (is.null(fnt) || length(fnt$columns) == 0) return("")
    if (length(fnt$columns) == 1 && fnt$columns[[1]] == "other") return("")

    fnt$value %>%
      str_match_all("9903\\.88\\.[0-9]{2}") %>%
      purrr::map(~ .x[, 1]) %>%
      purrr::flatten_chr()
  }

  footnotes %>%
    purrr::map(single_footnote) %>%
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
  dt[, tariff_list := {
    ifelse(str_detect(china_tariff, "9903\\.88\\.01"), "9903.88.01",
    ifelse(str_detect(china_tariff, "9903\\.88\\.02"), "9903.88.02",
    ifelse(str_detect(china_tariff, "9903\\.88\\.03"), "9903.88.03",
    ifelse(str_detect(china_tariff, "9903\\.88\\.15"), "9903.88.15", ""))))
  }]

  dt[, exempted := {
    stringr::str_split(china_tariff, '\t') %>%
    purrr::map_lgl(~ {
      other <- setdiff(.x[str_detect(.x, "9903\\.88\\.[0-9]{2}")], TARIFF_LIST)
      length(other) > 0
    })
  }]

  dt
}

#' @export
get_tariff_info_by <- function(year, vers = 0) {
  purrr::map_dfr(read_data(year, vers), parse_item) %>%
  as.data.table()
}

re <- get_tariff_info_by(2019)
setDT(re)

