box::use(magrittr[`%>%`, `%<>%`, `%T>%`])
box::use(stringr)
box::use(data.table[...])

NUTSTORE <- Sys.getenv("NUTSTORE")
DATA_LIB <- file.path(NUTSTORE, "Data", "国际数据", "美国贸易数据") 
trade <- list(
    import = list(
        china = file.path(DATA_LIB, "out", "use_import_china.Rds")
    )
)

#' Get import data from China
#'
#' This function reads and returns import data from China.
#'
#' @return A data table containing import data from China
#' @export
get_import_from_china <- function() {
    data <- readRDS(trade$import$china)
}



