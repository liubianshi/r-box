box::use(magrittr[`%>%`, `%<>%`, `%T>%`])
box::use(stringr[glue = str_glue])
box::use(data.table[setDT, as.data.table, data.table, setnames])
box::use(comtradr)
box::use(concordance[concord])

#' @export
get_monthly_trade_data <- function(reporter, start, end,
                                   flow = c("Export", "Import"),
                                   partner = "World",
                                   verbose = TRUE) {
  datalist <- vector('list', end - start + 1)
  for (year in start:end) {
    datalist[[year - start + 1]] <- local({
      re <- comtradr::ct_get_data(
        commodity_code = "TOTAL",
        report = reporter,
        partner = partner,
        flow_direction = flow,
        frequency = "M",
        start_date = glue("{year}-01"),
        end_date = glue("{year}-12"),
        verbose = verbose,
        cache = TRUE
      )
      if (all(dim(re) == c(1L, 1L))) return(NULL)
      setDT(re)[, .(
        year = ref_year,         month = ref_month,     period,
        reporter = reporter_iso, partner = partner_iso,
        flow = flow_desc,        value = primary_value
      )]
    })
  }

  data.table::rbindlist(datalist) %>%
  data.table::dcast(reporter + partner + year + month ~ flow)
}

#' @export
get_anual_trade_data <- function(reporter, start, end,
                                 flow = c("Export", "Import"),
                                 partner = "World",
                                 verbose = TRUE) {
  datalist <- vector('list', ceiling((end - start + 1) / 12))
  for (i in seq_along(datalist)) {
    datalist[[i]] <-
      comtradr::ct_get_data(
        commodity_code = "TOTAL",
        report = reporter,
        partner = partner,
        flow_direction = flow,
        frequency = "A",
        start_date = start + (i - 1) * 12,
        end_date = min(c(end, start + i * 12 - 1)),
        verbose = verbose,
        cache = TRUE
      ) %>%
      setDT() %>%
      .[, .(
        year = ref_year,  reporter = reporter_iso, partner = partner_iso,
        flow = flow_desc, value    = primary_value
      )]
  }
  data.table::rbindlist(datalist) %>%
  data.table::dcast(reporter + partner + year ~ flow)
}

#' @export
get_data_by_products <- function(reporter, year, flow,
                                 partner = "World",
                                 dest = c("HS", 6)) {
  stopifnot(length(flow) == 1)
  data <- comtradr::ct_get_data(
    commodity_code = "everything",
    report = reporter,
    partner = partner,
    flow_direction = flow,
    start_date = min(year),
    end_date = max(year),
    verbose = TRUE,
    cache = TRUE
  ) %>% setDT()

  data <- data[aggr_level == 6, .(
    reporter_iso,
    partner_iso,
    period,
    cmd_class = stringr::str_replace(classification_code, "H([0-9])", "HS\\1"),
    product = cmd_code,
    value = primary_value,
    weight = net_wgt
  )]

  origin <- unique(data$cmd_class)
  stopifnot(length(origin) == 1)
  dest_digits <-  dest[2]
  dest <-  dest[1]
  ids <- c("reporter_iso", "partner_iso", "period", "cmd_class", "product")
  if ((origin == dest || dest == "HS") && dest_digits == 6) return(data)
  if (origin == dest || dest == "HS") {
    dt[, product := stringr::str_sub(product, 1, dest_digits)] %>%
    .[, lapply(.SD, sum, na.rm = FALSE), by = ids, .SDcols = c("value", "weight") ] %>%
    return()
  }

  con <-
    concord(unique(data$product), origin, dest, dest_digits, all = TRUE) %>%
    purrr::imap_dfr(~ data.table(product = .y, match = .x$match, share = .x$weight))

  data <-
    data[con, on = "product", nomatch = NULL] %>%
    .[, product := match] %>%
    .[, cmd_class := dest] %>%
    .[, value := value * share] %>%
    .[, weight := weight * share] %>%
    .[, N := .N, by = ids] %>%
    .[N > 1, value := sum(value, na.rm = FALSE), by = ids] %>%
    .[N > 1, weight := sum(weight, na.rm = FALSE), by = ids] %>%
    .[, .(value = value[1], weight = weight[1]), by = ids]

  return(data)
}


