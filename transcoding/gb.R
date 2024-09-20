box::use(magrittr[`%>%`, `%<>%`, `%T>%`])
box::use(stringr[glue = str_glue])
box::use(data.table[setDT, as.data.table, data.table, setnames])
box::use(lbs)
box::use(utils/cache)
box::use(cachem)
box::use(digest[digest])

MERGE_PATH <- list(
  ISIC3_GB11 = c("ISIC3_GB02", "GB02_GB11"),
  GB11_ISIC3 = c("GB11_GB02",  "GB02_ISIC3"),
  GB02_GB17  = c("GB02_GB11",  "GB11_GB17"),
  GB17_GB02  = c("GB17_GB11",  "GB11_GB02"),
  ISIC3_GB17 = c("ISIC3_GB11", "GB11_GB17"),
  GB17_ISIC3 = c("GB17_GB11",  "GB11_ISIC3")
)

format_code <- function(code) {
  digits <- data.table(digits = nchar(code))
  digits_table <- digits[, .N, by = digits] %>% data.table::setorder(-N)
  stopifnot(digits_table[1, N] == max(digits_table$N, na.rm = TRUE))
  paste0(strrep("0", digits_table$digits[1] - digits$digits), code)
}

#' @export
get_gbtable <- function(orig, dest) {
  orig <- toupper(orig)
  dest <- toupper(dest)

  re <- tryCatch(
    lbs::getDataSQLite("Coding_Inds", glue("{orig}_{dest}")),
    error = function(e) {
      if (e$message == "isTRUE(table %in% tableList) is not TRUE") {
        lbs::getDataSQLite("Coding_Inds", glue("{dest}_{orig}"))
      }
    }
  )

  re <- re[, .SD, .SDcols = c(glue("{orig}_code"), glue("{dest}_code"))] %>%
    setnames(c("orig", "dest")) %>%
    .[, lapply(.SD, format_code)] %>%
    setnames(c(orig, dest))
  return(re)
}

concord_direct <- function(sourcevar, tbl, digit = 4) {
  stopifnot(inherits(tbl, "data.frame") && length(tbl) == 2)
  orig_names <- names(tbl)
  tbl <- as.data.table(tbl) %>% setnames(c("orig", "dest"))
  re <- purrr::map(sourcevar, ~ {
    .data <- tbl[
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

  attr(re, "origin") <- orig_names[1]
  attr(re, "destination") <- orig_names[2]

  re
}

tbl_combine <- function(re1, re2) {
  stopifnot(attr(re1, "destination") == attr(re2, "origin"))

  match_table <- purrr::imap_dfr(re2, ~ {
    data.frame(orig = .y, match = .x$match, weight = .x$weight)
  }) %>% setDT()

  re <- purrr::map(re1, ~ {
    if (is.na(.x$match)[1]) return(.x)

    data.table(orig = .x$match, weight_pre = .x$weight) %>%
    match_table[., on = "orig"] %>%
    .[, weight := weight_pre * weight] %>%
    .[, .(weight = sum(weight)), by = match] %>%
    as.list()
  })

  names(re) <- names(re1)
  attr(re, "origin") = attr(re1, "origin")
  attr(re, "destination") = attr(re2, "destination")

  re
}


#' @title Convert codes between different coding systems
#' @description This function converts codes between different coding systems
#' using concordance package.
#' 
#' @param sourcevar A character vector of codes to be converted.
#' @param origin The origin coding system.
#' @param destination The destination coding system.
#' @param dest.digit The number of digits in the destination coding
#' system. Default is NULL.
#' 
#' @return A list of converted codes.
#' 
#' @export
concord <- function(sourcevar, origin, destination, dest.digit = NULL) {
  stopifnot(
    is.character(sourcevar)
    && is.character(origin)
    && is.character(destination)
    && length(origin) == 1
    && length(destination) == 1
  )

  orig <- toupper(origin)
  dest <- toupper(destination)

  # 除涉及国民经济行业编码的转换外，其他转换使用 concordance::concord
  if (
    !stringr::str_detect(orig, "^GB") 
    && !stringr::str_detect(dest, "^GB")
  ) {
    box::use(concordance)
    re <- if (is.null(dest.digit)) {
      concordance$concord(sourcevar, origin, destination, all = TRUE)
    } else {
      concordance$concord(sourcevar, origin, destination, dest.digit, all = TRUE)
    }

    attr(re, "origin") = orig
    attr(re, "destination") = dest
    return(re)
  }

  if (is.null(dest.digit)) dest.digit <- 4
  # 除明确标示了转换路径的编码转换外，假设其他编码转换可以直接进行
  if (!glue("{orig}_{dest}") %in% names(MERGE_PATH)) {
    return(concord_direct(sourcevar, get_gbtable(orig, dest), dest.digit))
  }

  re <- NULL
  path <-
    MERGE_PATH[[glue("{orig}_{dest}")]] %>%
    purrr::map(~ stringr::str_split_fixed(.x, "_", 2)[1, ])

  for (i in seq_along(path)) {
    orig_inter <- path[[i]][1]
    dest_inter <- path[[i]][2]
    if (is.null(re)) {
      re <- concord(sourcevar, orig_inter, dest_inter)
      next
    }

    matched_codes <- purrr::map(re, "match") %>% purrr::flatten_chr() %>% unique()
    new_match <- if (i == length(path)) {
      concord(matched_codes, orig_inter, dest_inter, dest.digit)
    } else {
      concord(matched_codes, orig_inter, dest_inter)
    }

    re <- tbl_combine(re, new_match)
  }

  return(re)
}

generate_concord_tables <- function(
  envir, sourcevar, data_code, target_code,
  data_code_digit, target_code_digit, index_type
) { 
  cacheid <- # 根据选项计算缓存 ID，方便在使用 purrr 时直接调用缓存的数据
    c(sourcevar, data_code, target_code, data_code_digit, target_code_digit, index_type) %>%
    paste0(collapse = "") %>%
    digest(serialize = FALSE) %>%
    paste0("cache_", .)

  re <- cache$object(
    cacheid,
    {
      from_traget <- concord(sourcevar, target_code, data_code, data_code_digit)
      to_target <- if (index_type == "level") {
        purrr::map(from_traget, "match") %>%
        purrr::flatten_chr() %>%
        unique() %>%
        concord(data_code, target_code, target_code_digit)
      } else {
        NULL
      }
      list(from_traget = from_traget, to_target = to_target)
    },
    envir
  )

  on.exit({
    # 如何从全局环境直接调用此函数，那么在函数退出时清理缓存变量，
    # 以免污染全局环境
    if (identical(parent.frame(), .GlobalEnv)) {
      rm(cacheid, envir = .GlobalEnv)
    }
  })

  return(re)
}


#' Calculate index based on source variable and query data
#'
#' This function calculates an index based on the source variable and query data provided.
#'
#' @param sourcevar The source variable to use for calculating the index
#' @param data_code The data code to match in the source variable
#' @param target_code The target code to match in the source variable
#' @param query_data The query data to use for calculating the index
#' @param index_type The type of index to calculate, either "level" or "rate" (default is "level")
#' @param data_code_digit The digit for the data code (default is NULL)
#' @param target_code_digit The digit for the target code (default is NULL)
#' @return A list of calculated index values for each source variable
#' @export
cal_index <- function(
  sourcevar, data_code, target_code, query_data,
  index_type = "level", # or rate
  data_code_digit = NULL,
  target_code_digit = NULL,
  na.rm = FALSE
) {
  stopifnot(
    inherits(query_data, "data.frame")
    && length(query_data) == 2L
    && !anyDuplicated(query_data[[1]])
    && index_type %in% c("level", "rate")
  )
  names(query_data) <- c("match", "value")

  concord_table <- generate_concord_tables(
    parent.frame(),
    sourcevar, data_code, target_code, data_code_digit,
    target_code_digit, index_type
  )

  re <- purrr::map_dbl(sourcevar, function(code) {
    if (is.na(concord_table$from_traget[[code]]$match)[1]) return(NA)
    .data <- as.data.table(concord_table$from_traget[[code]]) %>%
      query_data[., on = "match", nomatch = NA]
    value_adj <- 1
    if (index_type == "level") {
      value_adj <- concord_table$to_target[.data[, match]] %>%
        purrr::map_dbl(~ .x$weight[.x$match == code])
    }
    .data[, .(sum(value * value_adj * weight, na.rm = na.rm))][[1]]
  })

  names(re) <- sourcevar
  re
}

