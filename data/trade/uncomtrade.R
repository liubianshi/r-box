box::use(magrittr[`%>%`, `%<>%`, `%T>%`])
box::use(stringr[glue = str_glue])
box::use(data.table[setDT, as.data.table, data.table, setnames])
box::use(comtradr)
box::use(concordance)

recal_value <- function(value, products, origin, dest, dest_digits) {
    stopifnot(length(dest_digits) == 1L && dest_digits %in% c(2, 4, 6))
    stopifnot(all(!is.na(products)))
    stopifnot(anyDuplicated(products) == 0)
    origin <- unique(origin)
    stopifnot(length(origin) == 1)
    origin <- stringr::str_replace(origin, "H([0-9])", "HS\\1")

    dt <- data.table(product = products, value = value, cmd_class = origin)
    if ((origin == dest || dest == "HS") && dest_digits == 6) return(dt)
    if (origin == dest || dest == "HS") {
        dt[,
            .(value = sum(value, na.rm = TRUE), cmd_class = origin),
            by = .(product = stringr::str_sub(product, 1, dest_digits))
        ] %>%
        return()
    }

    get_value <- function(p) dt[product == p, value]

    dt <-
        concordance::concord(products, origin, dest, dest_digits, all = TRUE) %>%
        purrr::imap_dfr(~ {
            data.table(product = .x$match, value = get_value(.y) * .x$weight)
        })

    dt[!is.na(product),
        .(value = sum(value, na.rm = TRUE), cmd_class = dest),
        by = product
    ]
}


#' @export
get_data_by_products <- function(reporter, year, flow,
                                 partner = "World",
                                 dest = c("HS", 6)) {
    stopifnot(length(flow) == 1)
    colnames <- c(
        "classification_code", "cmd_code", "reporter_iso",
        "partner_iso",         "period",   "primary_value"
    )
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
    data <- data[aggr_level == 6, .SD, .SDcols = colnames]
    data <- data[,
        j  = recal_value(primary_value, cmd_code, classification_code, dest[1], dest[2]),
        by = .(reporter_iso, partner_iso, period)
    ]
    return(data)
}





