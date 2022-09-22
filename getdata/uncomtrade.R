collapse_seq_per_step <- function(x, step = 5L, collapse = ",") # --------- {{{2
{
    if (length(x) == 0) stop("x is empty!", call. = FALSE)
    l <- length(x) %/% step
    if (length(x) %% step != 0) l = l + 1
    s <- vector("character", l)
    for (i in seq_len(l)) {
        s[i] <- paste(x[(step * i + 1 - step):(min(step * i, length(x)))],
					  collapse = collapse)
    }
    s
}

#' @export
getdata <- function(reporter, partner, period, ...)
{   # 从 UN Comtrade 数据库获取贸易数据
    combine_query <- function(q) {
        stopifnot(purrr::none(q, is.null))
        stringr::str_c(q$url
            , "max=",  q$maxrec,          "&"  # maximum no. of records returned
            , "type=", q$type,            "&"  # type of trade (c=commodities)
            , "freq=", q$freq,            "&"  # frequency
            , "px=",   q$classification,  "&"  # classification
            , "ps=",   q$period,          "&"  # time period
            , "r=",    q$reporter,        "&"  # reporting area
            , "p=",    q$partner,         "&"  # partner country
            , "rg=",   q$tradeflow,       "&"  # trade flow
            , "cc=",   q$code,            "&"  # classification code
            , "fmt=",  q$fmt                   # Format
        )
    }
    ife <- function(key, value, p) {
        lbs::ifthen(p[[key]], value)
    }

    params <- c(reporter = reporter, partner = partner, period = period, list(...))
    query_default <- list(
        reporter = NULL,         # reporting area
        partner = NULL,         # partner area
        period = NULL,         # time period
        url = "http://comtrade.un.org/api/get?",
        maxrec = 50000,        # maximum no. of records returned
        freq = "A",            # frequency
        type = "C",            # type of trade
        classification = "HS", # classification
        tradeflow = "all",     # tradeflow
        code = "TOTAL",        # classification code
        fmt = "json"            # format
    )
    query <- purrr::map2(names(query_default), query_default, ife, params)
    names(query) <- names(query_default)
    print(q <- combine_query(query))
    if (query$fmt == "csv") {
        data.table::fread(q)
    } else if (query$fmt == "json") {
        result <- jsonlite::read_json(q)
        if (result$validation$status$value == 0 &
            result$validation$count$value > 0) {
            result$dataset <- data.table::rbindlist(result$dataset)
        }
        result
    }
}

#' @export
api <- function(field, ...) {
    l <- list(...)
    if (is.null(l)) {
        do.call(paste0("api_", field))
    } else {
        do.call(paste0("api_", field), l)
    }
}

# r (default = 0) reporting area: the area that reported the trade to UNSD. See
# list of valid reporters (JSON, formatted for select2.js, pass "id" to
# parameter).
api_reporter <- function() {
    api_url <- "https://comtrade.un.org/Data/cache/reporterAreas.json"
    data_json <- jsonlite::read_json(api_url)[["results"]]
    data <- data.table::data.table(
        id = purrr::map_chr(data_json, "id"),
        name = purrr::map_chr(data_json, "text")
    )
    data

}

# p partner area (default = all): partner area. The area receiving the trade,
# based on the reporting areas data. See list of valid partners: (JSON,
# formatted for select2.js, pass "id" to parameter)
api_partner <- function() {
    api_url <- "https://comtrade.un.org/Data/cache/partnerAreas.json"
    data_json <- jsonlite::read_json(api_url)[["results"]]
    data <- data.table::data.table(
        id = purrr::map_chr(data_json, "id"),
        name = purrr::map_chr(data_json, "text")
    )
    data
}


# ps time period (default = now): Depending on freq, time period can take
# either YYYY or YYYYMM or now or recent. For example
api_period <- function() {
    message("
    ps time period (default = now): Depending on freq, time period can take
    either YYYY or YYYYMM or now or recent. For example

    Annual (YYYY): 2010

    Monthly (YYYY or YYYYMM): Individual periods as 201001 or full years as
    2010, automatically expands to query periods 201001,201002,201003,...,
    201012.

    The special code now can be used to ask for the most recently available
    period. For annual data this might be 2013, for monthly it will be the most
    recent single month. Depending on reporter data may not be available.

    The special code recent can be used to ask for the 5 most recently
    available periods, e.g. 2013,2012,2010,2009,2008 for annual or 201401,
    201312,201311, 201310,201309.
    ")
}

# px classification (default values are HS for goods and EB02 for services):
# Trade data classification scheme. See list of valid classifications:
api_classification <- function(get_classification_code = FALSE) {
    if (!isFALSE(get_classification_code) && length(get_classification_code) > 1) {
        return("代码参数设置错误")
    }

    message('
        HINT: data is not always available in every
        classification. Availability varies by time period and reporter. A good
        default is HS, meaning "Harmonized System (HS), as reported", whereas
        a service default is EB02, meaning "Extended Balance of Payments
        Services Classification". To see what data is available in different
        classifications see the data availability section.

        WARNING: only older data was submitted to UN Comtrade in SITC. With
        rare exceptions no data will be returned in queries from 1992 or later
        if code ST, meaning "Standard International Trade Classification (SITC),
        as reported" is chosen. If you require data formatted in the SITC
        classification, it is better to use S1 for SITC Rev. 1 if you would
        like the longest time series or check the data availability section and
        use the SITC revision most appropriate for the data you are interested
        in.
    ')

    code <- data.table::fread("
        code	name	url
        HS	Harmonized System (HS)	https://comtrade.un.org/Data/cache/classificationHS.json
        H0	HS 1992	https://comtrade.un.org/Data/cache/classificationH0.json
        H1	HS 1996	https://comtrade.un.org/Data/cache/classificationH1.json
        H2	HS 2002	https://comtrade.un.org/Data/cache/classificationH2.json
        H3	HS 2007	https://comtrade.un.org/Data/cache/classificationH3.json
        H4	HS 2012	https://comtrade.un.org/Data/cache/classificationH4.json
        ST	Standard International Trade Classification (SITC), as reported	https://comtrade.un.org/Data/cache/classificationST.json
        S1	SITC Revision 1	https://comtrade.un.org/Data/cache/classificationS1.json
        S2	SITC Revision 2	https://comtrade.un.org/Data/cache/classificationS2.json
        S3	SITC Revision 3	https://comtrade.un.org/Data/cache/classificationS3.json
        S4	SITC Revision 4	https://comtrade.un.org/Data/cache/classificationS4.json
        BEC	Broad Economic Categories (BEC)	https://comtrade.un.org/Data/cache/classificationBEC.json
        EB02	Extended Balance of Payments Services Classification (EB02)	https://comtrade.un.org/Data/cache/classificationEB02.json
        ")

    if (isFALSE(get_classification_code)) {
        code[, .(code, name)]
    } else {
        url <- code[code == get_classification_code, url]
        data_json <- jsonlite::read_json(url)[["results"]]
        data <- data.table::data.table(
            id = purrr::map_chr(data_json, "id"),
            name = purrr::map_chr(data_json, "text")
        )
        l <- list(code[, .(code, name)], data)
        names(l) <- c("codelist", get_classification_code)
        invisible(l)
    }
}

#' @export
getdata_by_country_hs6_year <- function(year, reporterlist = NULL, replace = FALSE)
{   # 利用 UNComtrade 的 API 下载 UNCOMTRSDE 数据
    reporterlist <- if (is.null(reporterlist)) {
        api("reporter")[stringr::str_detect(id, "^\\d+$")][[1]]
    } else {
        unique(reporterlist)
    }
    validation <- vector("list", length(reporterlist))
    importvalue <- vector("list")

    for (r in reporterlist) {
        print(gettextf("--------- present reporter is: %s ---------", r))

        result <- getdata(reporter  = r,
                          partner    = "0",
                          tradeflow = "1",
                          code      = "AG6",
                          period    = year)
        
        validation[[r]] <- result$validation
        if (result$validation$status$value == 0 &
            result$validation$count$value > 0) {
            importvalue[[r]] <- result$dataset
        }
    }
    return(list(validation = validation, importvalue = importvalue))
}

