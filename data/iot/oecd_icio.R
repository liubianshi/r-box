box::use(magrittr[`%>%`, `%<>%`, `%T>%`])
box::use(stringr[glue = str_glue])
box::use(data.table[setDT, as.data.table, data.table])
box::use(readxl)
box::use(rlang)

# system(glue("exa -T {DATA_LIB}"))
DATA_LIB <- file.path(Sys.getenv("DATA_LIB"), "Sync_Data", "OECD_ICIO_2023")
YEAR_BEGIN <- 1995
YEAR_END <- 2020

#' Get world export by industry
#' @param year A num vector
#' @return A list
#' @export
get_inds_export_share <- function(year, country = "World") {
    stopifnot(all(year >= YEAR_BEGIN & year <= YEAR_END))
    stopifnot(length(country) == 1L)

    load(file.path(DATA_LIB, "sources", "FD.RData"))
    year_list <- match(year, attr(ICIO2023econFD, "dimnames")[[1]])
    country_list <- attr(ICIO2023econFD, "dimnames")[[3]]
    inds_list <- stringr::str_match(attr(ICIO2023econFD, "dimnames")[[2]], "^[^_]*_(.*)$")[, 2] %>%
        unique()
    country_inds_list <- local({
        country_inds <- attr(ICIO2023econFD, "dimnames")[[2]]
        if (country == "World") return("ALL")

        stringr::str_split_fixed(country_inds, "_", 2) %>%
        as.data.table() %>%
        data.table::setnames(c("country", "inds")) %>%
        .[, row := .I] %>%
        .[country %in% c("CN1", "CN2"), country := "CHN"] %>%
        .[country %in% c("MX1", "MX2"), country := "MEX"] %>%
        .[country %in% evalq(country, rlang::env_parent(n = 1)), row] %>%
        country_inds[.]
    })

    load(file.path(DATA_LIB, "sources", "Z.RData"))
    m_collapse_by_country <- vapply(1:81, function(.x) {
        c(rep(0, (.x - 1) * 45), rep(1, 45), rep(0, (81 - .x) * 45))
    }, rep(0, 81 * 45))

    sum_by_inds <- function(M, by = "inds") {
        M <- if (by == "inds") {
            M[,
                lapply(.SD, sum),
                by = .(inds = stringr::str_match(rn, "^[A-z0-9]+_(\\w+)")[, 2]),
                .SDcols = country_list
                ] %>%
                as.matrix(rownames = "inds")
        } else {
            M[, lapply(.SD, sum), by = rn, .SDcols = country_list] %>%
            as.matrix(rownames = "rn")
        }

        M %*% matrix(rep(1, 81), ncol = 1) %>% as.numeric()
    }

    re <- purrr::map(year_list, function(y) {
        Z  <- ICIO2023econZ[y, , ] %*% m_collapse_by_country
        colnames(Z) <- country_list
        FD <- ICIO2023econFD[y, , ]

        Z_FD <- as.data.table(Z + FD, keep.rownames = TRUE)
        if (length(country_inds_list) != 1 || country_inds_list != "ALL") {
            Z_FD <- Z_FD[rn %in% country_inds_list]
        }
        X <- data.table::copy(Z_FD)
        purrr::walk(country_list, ~ {
            if (stringr::str_detect(.x, "^(MX[1-2]|MEX)")) {
                X[stringr::str_detect(rn, "^(MX|MEX)"), c(.x) := 0]
            } else if (stringr::str_detect(.x, "^(CN[1-2]|CHN)")) {
                X[stringr::str_detect(rn, "^(CN[1-2]|CHN)"), c(.x) := 0]
            } else {
                X[stringr::str_detect(rn, glue("^{.x}")), c(.x) := 0]
            }
        })
        X <- sum_by_inds(X)
        O <- sum_by_inds(Z_FD)
        X_O <- ifelse(is.nan(X / O), 0, X / O)
        names(X_O) <- inds_list
        return(X_O)
    })
    names(re) <- year
    return(re)
}

#' Get industry ouput
#' @param year A integer vector
#' @param Country A Character vector  or "World"
#' @export
get_inds_output <- function(year, country = "World") {
    stopifnot(all(year >= YEAR_BEGIN & year <= YEAR_END))
    ori_data_path <- file.path(DATA_LIB, "sources", "X.RData")
    load(ori_data_path)

    X <- t(ICIO2023econX) %>% as.data.table(keep.rownames = TRUE)
    X[, c("country", "inds") := as.data.table(stringr::str_split_fixed(rn, "_", 2))]
    X <- X[, .SD, .SDcols = c("country", "inds", year)]

    if (length(country) == 1 && country == "World") {
        X[, purrr::map(.SD, sum, na.rm = TRUE), by = "inds", .SDcols = as.character(year)] %>%
            return()
    } else {
        X[country %in% evalq(country, rlang::env_parent(n = 1))] %>%
            return()
    }
}

#' Get conversion table between ICIO and ISIC Rev.4 codes
#'
#' This function reads a Excel file containing the conversion table between ICIO and ISIC Rev.4 codes
#' and returns a data table with the corresponding mappings.
#'
#' @return A data table with two columns: 'icio' for ICIO codes and 'isic4' for ISIC Rev.4 codes.
#' @import readxl
#' @import stringr
#' @import purrr
#' @import data.table
#' @export
#'
get_conversion_table_icio_isic4 <- function() {
    ori_data_path <- file.path(DATA_LIB, "sources", "ReadMe_ICIO_Robjects for time series analysis.xlsx")
    data <- readxl::read_xlsx(
        ori_data_path,
        sheet = "Country_Industry",
        range = "I3:K48"
    )
    setDT(data)

    table <- purrr::map2_dfr(
        data$Code,
        stringr::str_split(data[["ISIC Rev.4"]], ",\\s*"),
        function(code, inds) {
            inds <- purrr::map_chr(inds, ~ {
                if (stringr::str_detect(.x, " to ")) {
                    range <- stringr::str_match(.x, "([0-9]+)\\s*to\\s*([0-9]+)")[1, 2:3]
                    paste(range[1]:range[2], collapse = ", ")
                } else {
                    .x
                }
            }) %>%
                paste(collapse =  ", ")
            inds <- stringr::str_split(inds, ", ")[[1]]
            data.table(icio = code, isic4 = inds)
        }
    )
    return(table)
}

#' @export
get_country_list <- function() {
    DATA_LIB <- file.path(Sys.getenv("DATA_LIB"), "Sync_Data", "OECD_ICIO_2023")
    ori_data_path <- file.path(DATA_LIB, "sources", "ReadMe_ICIO_Robjects for time series analysis.xlsx")
    data <- local({
        ori <- list(path = ori_data_path,  sheet = "Country_Industry", col_names = FALSE)
        purrr::map(c("C4:D43", "F4:G43"), ~ do.call(readxl::read_xlsx, c(ori, range = .x))) %>%
        data.table::rbindlist() %>%
        data.table::setnames(c("country_iso", "country_name"))
    })
    new <- c("BGD", "BLR", "CMR", "CIV", "EGY", "JOR", "NGA", "PAK", "SEN", "UKR")
    data[, new := country_iso %in% new]
}


#' Generate Industry-specific Input-Output Table
#'
#' This function generates an industry-specific input-output table based on the given input-output table.
#'
#' @param wiot Input-output table
#'
#' @return Industry-specific input-output table
#'
#' @examples
#' wiot <- matrix(1:9, nrow = 3, ncol = 3, dimnames = list(c("A", "B", "C"), c("A_s", "B_s", "C_s")))
#' gen_industry_iot(wiot)
#'
#' @export
gen_industry_iot <- function(year) {
    stopifnot(length(year) == 1)
    load(file.path(DATA_LIB, "sources", "Z.RData"))
    wiot <- ICIO2023econZ[match(year, attr(ICIO2023econZ, "dimnames")[[1]]), , ]

    sector_list <-
        stringr::str_replace_all(attr(wiot, "dimnames")[[2]], "(\\w+)_(\\w+)", "\\2") %>%
        unique()
    names(sector_list) <- sector_list
    multi_combine_column <- sector_list %>%
        purrr::map_dfc(~ {
            stringr::str_detect(attr(wiot, "dimnames")[[2]], sprintf("_%s$", .x)) %>%
            as.integer()
        }) %>%
        as.matrix()
    m <- t(multi_combine_column) %*% wiot %*% multi_combine_column
    rownames(m) <- names(sector_list)
    colnames(m) <- names(sector_list)
    return(m)
}


#' Generate coefficient table
#'
#' This function generates a coefficient table based on the input data.
#'
#' @param iiot Input data, a matrix or data frame
#' @return Coefficient table
#'
#' @examples
#' iiot <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' gen_coef_table(iiot)
#' @export
gen_coef_table <- function(iiot = NULL, year = NULL) {
    stopifnot(! (is.null(iiot) && is.null(year)))
    if (is.null(iiot)) iiot <- gen_industry_iot(year)
    rnames <- rownames(iiot)
    cnames <- colnames(iiot)
    as_input_total <- apply(iiot, 1, sum)
    multi <- diag(1 / as_input_total, dim(iiot)[1])
    m <- multi %*% iiot
    m[is.nan(m)] <- 0
    rownames(m) <- rnames
    colnames(m) <- cnames
    m
}
