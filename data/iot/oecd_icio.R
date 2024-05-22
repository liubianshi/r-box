box::use(magrittr[`%>%`, `%<>%`, `%T>%`])
box::use(stringr)
box::use(data.table[setDT, as.data.table, data.table])
box::use(readxl)

DATA_LIB <- '/Users/luowei/Documents/Data/OECD_ICIO_2023'

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
                    range <- stringr::str_match(.x, '([0-9]+)\\s*to\\s*([0-9]+)')[1, 2:3]
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
gen_industry_iot <- function(wiot) {
    sector_list <- 
        stringr::str_replace_all(attr(wiot, "dimnames")[[2]] , "(\\w+)_(\\w+)", "\\2") %>%
        unique()
    names(sector_list) = sector_list
    multi_combine_column <- purrr::map_dfc(sector_list, ~ {
        stringr::str_detect(attr(wiot, "dimnames")[[2]], sprintf('_%s$', .x)) %>%
        as.integer()
    }) %>% as.matrix()
    t(multi_combine_column) %*% wiot %*% multi_combine_column
}


#' Generate coefficient table
#'
#' This function generates a coefficient table based on the input data.
#'
#' @param iiot Input data, a matrix or data frame
#' @return Coefficient table
#' @export
#'
#' @examples
#' iiot <- matrix(c(1,2,3,4,5,6), nrow = 2)
#' gen_coef_table(iiot)
gen_coef_table <- function(iiot) {
    as_input_total <- apply(iiot, 1, sum)
    multi <- diag(1/as_input_total, dim(iiot)[1])
    multi %*% iiot 
}


