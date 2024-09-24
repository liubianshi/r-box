# Packages
box::use(utils)
box::use(curl)
box::use(stringr[str_split_fixed, glue = str_glue, str_replace])
box::use(purrr)

client <- system2(file.path(Sys.getenv("HOME"), ".private_info.sh"), "unctad", stdout = TRUE)
client_id     <- str_split_fixed(client, "\t", 2)[1, 1]
client_secret <- str_split_fixed(client, "\t", 2)[1, 2]
temp_file_path <- "synchrone.csv.gz"
on.exit(if (file.exists(temp_file_path)) file.remove(temp_file_path), add = TRUE)

get_index_meta <- function(x) {
  stopifnot(length(x) == 1L)
  switch(x,
    ftri = , frontier_technology_readiness_index = list(
      url = "https://unctadstat-user-api.unctad.org/US.FTRI/cur/Facts?culture=en",
      field = "M6700",
      compute = "round(M6700/Value div 1, 1)",
      name = "Index"
    ),
    percentage_of_total_world = list(
      url = "https://unctadstat-user-api.unctad.org/US.TradeMerchTotal/cur/Facts?culture=en",
      field = "M5011",
      compute = "round(M5011/Value div 1, 3)",
      name = "Percentage_of_total_world"
    ),
    NULL
  )
}

get_index_info <- function(index_list, info) {
  index_info <- purrr::map(index_list, function(index) {
    index <- tolower(index)
    meta <- get_index_meta(index)
    if (!is.null(meta)) {
      return(list(
        list(
          name = glue("{meta$name}_Value"),
          compute = glue("{meta$field}/Value"),
          class = "double"
        ),
        list(
          name = glue("{meta$name}_Footnote"),
          compute = glue("{meta$field}/Footnote/Text"),
          class = "character"
        ),
        list(
          name = glue("{meta$name}_MissingValue"),
          compute = glue("{meta$field}/MissingValue/Label"),
          class = "character"
        )
      ))
    }

    if (index == "year") {
      return(list(
        code = "Year",
        name = "Year",
        class = "integer",
        order = "Year asc"
      ))
    }

    if (index %in% c('economy', 'flow', 'category')) {
      index <- str_replace(index, "^\\w", toupper)
      return(list(
        code = glue("{index}/Code"),
        name = glue("{index}/Label"),
        class = "character",
        order = glue("{index}/Order asc")
      ))
    }

    return(NULL)
  })

  index_info <- purrr::map(index_info, ~ if (is.null(.x$name)) .x else list(.x))
  index_info <- do.call(c, index_info)

  names(info) <- info
  purrr::map(info, function(o) {
    switch(o,
      code = purrr::map_chr(index_info, "code"),
      name = purrr::map_chr(index_info, "name"),
      select = local({
        purrr::map_chr(index_info, ~ {
          if (is.null(.x$code) || .x$name == "Year") .x$name
          else glue("{.x$code},{.x$name}")
        }) |> paste0(collapse = ",")
      }),
      compute = index_info |>
        purrr::discard(~ is.null(.x$compute)) |>
        purrr::map_chr(~ glue("{.x$compute} as {.x$name}")) |>
        paste(collapse = ", "),
      class = purrr::map_chr(index_info, "class"),
      orderby = index_info |>
        purrr::discard(~ is.null(.x$order)) |>
        purrr::map_chr("order") |>
        paste(collapse = ", "),
      NULL
    )
  })
}

gen_filter <- function(...) {
  args <- list(...) |> purrr::discard(~is.null(.x))
  purrr::imap_chr(args, function(values, key) {
    info <- get_index_info(key, c("code", "class"))
    qt <- if (info$class == "integer") "" else "'"
    gettextf("%s in (%s)", info$code, paste0(qt, values, qt, collapse = ","))
  }) |>
  paste(collapse = " and ")
}

#' @export
get <- function(economy, year, index, ..., format = "csv", compress = "gz") {
  filter_args   <- list(economy = economy, ..., year = year)
  filter <- do.call(gen_filter, filter_args)
  index_info <- get_index_info(
    c(names(filter_args), index),
    c("select", "compute", "orderby", "class")
  )

  curl_handle <- curl::new_handle() |>
    curl::handle_setform(
      "$select"  = index_info$select,
      "$filter"  = filter,
      "$orderby" = index_info$orderby,
      "$compute" = index_info$compute,
      "$format"  = format,
      "compress" = compress
    ) |>
    curl::handle_setheaders(ClientId = client_id, ClientSecret = client_secret)

  curl::curl_download(get_index_meta(index[1])$url, temp_file_path, handle = curl_handle)
  data <- utils::read.csv(
    gzfile(temp_file_path),
    header = TRUE,
    na.strings = "",
    encoding = "UTF-8"
  )
}

