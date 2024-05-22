box::use(comtradr[ct_get_data])

get_goods_import <- function(reporter, parter, comodity = "TOTAL")

data <- comtradr::ct_get_ref_table('HS')

data <- ct_get_data(
    commodity_code = "everything",
    report = "China",
    partner = "World",
    flow_direction = "Export",
    start_date = 2018,
    end_date = 2018,
    verbose = TRUE
)
