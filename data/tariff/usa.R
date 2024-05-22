box::use(magrittr[`%>%`, `%<>%`, `%T>%`])
box::use(stringr)
box::use(data.table[...])

NUTSTORE <- Sys.getenv("NUTSTORE")
DATA_LIB <- file.path(NUTSTORE, "Data", "国际数据") 
tariff <- list(
    china_extra = file.path(
        DATA_LIB,
        "美国对华加征关税清单",
        "out"
    )
)

#' Extra tariff U.S. added to commidity imported from china
#' @param hs A number, 6 for HS6, other for HTS 8 or 10
#' @return A data.table with hs, year, tariff
#' @export
china_extra <- function(hs = 6) {
    data <-
    if (hs == 6) {
        readRDS(file.path(tariff$china_extra, "weighted_tariff_add_hs6_2018-2020.Rds"))
    } else {
        readRDS(file.path(tariff$china_extra, "weighted_tariff_add_2018-2020.Rds"))
    }
    
    return(data)
}

