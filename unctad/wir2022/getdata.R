# vim: set nowrap fdm=marker:
box::use(magrittr[`%>%`])

ori <- list( # 原始数据的信息 =========================================== {{{1
    source = "https://unctad.org/topic/investment/world-investment-report",
    last_udpate_date = "2022-09-19",
    lib = "https://unctad.org/system/files/non-official-document/",
    cache = file.path(Sys.getenv("HOME"), ".cache", "R", "wir2022"),
    unitrow = 2L,
    mainkey = c(name = "region_economy", code = "code", no = "no"),
    time = "year",
    flow = list(        # 流量 ------------------------------------------ {{{2
        inward  = list(file = "WIR2022_tab01.xlsx",
                       desc = "Annex table 01: FDI inflows, by region and economy, 1990-2021"),
        outward = list(file = "WIR2022_tab02.xlsx",
                       desc = "Annex table 02: FDI outflows, by region and economy, 1990-2021")
    ),
    stock = list(       # 存量 ------------------------------------------ {{{2
        inward  = list(file = "WIR2022_tab03.xlsx",
                       desc = "Annex table 03: FDI inward stock, by region and economy, 1990-2021"),
        outward = list(file = "WIR2022_tab04.xlsx",
                       desc = "Annex table 04: FDI outward stock, by region and economy, 1990-2021")
    ),
    ma = list(          # 并购 ------------------------------------------ {{{2
        value = list(       # 按金额 ------------------------------------ {{{3
            sale = list(
                byregion = list(file = "WIR2022_tab05.xlsx",
                                desc = "Annex table 05: Value of net cross-border M&A sales, by region/economy of seller, 1990-2021"),
                bysector = list(file = "WIR2022_tab09.xlsx",
                                desc = "Annex table 09: Value of net cross-border M&A sales, by sector/industry, 1990-2021")
            ),
            purchase = list(
                byregion = list(file = "WIR2022_tab06.xlsx",
                                desc = "Annex table 06: Value of net cross-border M&A purchases, by region/economy of purchaser, 1990-2021"),
                bysector = list(file = "WIR2022_tab10.xlsx",
                                desc = "Annex table 10: Value of net cross-border M&A purchases, by sector/industry, 1990-2021")
            )
        ),
        number = list(      # 按数量 ----------------------------------------- {{{3
            sale = list(
                byregion = list(file = "WIR2022_tab07.xlsx",
                                desc = "Annex table 07: Number of net cross-border M&A sales, by region/economy of seller, 1990-2021"),
                bysector = list(file = "WIR2022_tab11.xlsx",
                                desc = "Annex table 11: Number of net cross-border M&A sales, by sector/industry, 1990-2021")
            ),
            purchase = list(
                byregion = list(file = "WIR2022_tab08.xlsx",
                                desc = "Annex table 08: Number of net cross-border M&A purchases, by region/economy of purchaser, 1990-2021"),
                bysector = list(file = "WIR2022_tab12.xlsx",
                                desc = "Annex table 12: Number of net cross-border M&A purchases, by sector/industry, 1990-2021")
            )
        )
    ),
    greenfield = list(  # 绿地投资 -------------------------------------- {{{2
        value = list( # 按项目金额 -------------------------------------- {{{3
            bysource      = list(file = "WIR2022_tab13.xlsx",
                                 desc = "Annex table 13: Value of announced greenfield FDI projects, by source, 2003-2021"),
            bydestination = list(file = "WIR2022_tab14.xlsx",
                                 desc = "Annex table 14: Value of announced greenfield FDI projects, by destination, 2003-2021"),
            bysector      = list(file = "WIR2022_tab15.xlsx",
                                 desc = "Annex table 15: Value of announced greenfield FDI projects, by sector/industry, 2003-2021")
        ),
        number = list( # 按项目数 --------------------------------------- {{{3
            bysource      = list(file = "WIR2022_tab16.xlsx",
                                 desc = "Annex table 16: Number of announced greenfield FDI projects, by source, 2003-2021"),
            bydestination = list(file = "WIR2022_tab17.xlsx",
                                 desc = "Annex table 17: Number of announced greenfield FDI projects, by destination, 2003-2021"),
            bysector      = list(file = "WIR2022_tab18.xlsx",
                                 desc = "Annex table 18: Number of announced greenfield FDI projects, by sector/industry, 2003-2021")
        )
    ),
    topmne = list(      # 最大的跨国公司 -------------------------------- {{{2
        all        = list(file = "WIR2022_tab19.xlsx",
                          desc = "Annex table 19: The world's top 100 non-financial MNEs, ranked by foreign assets, 2021"),
        developing = list(file = "WIR2022_tab20.xlsx",
                          desc = "Annex table 20: The top 100 non-financial MNEs from developing and transition economies, ranked by foreign assets, 2020")
    )
)
dir.create(ori$cache, recursive = TRUE, showWarnings = FALSE, mode = "0755")

# 经济体编码 ------------------------------------------------------------ {{{1
economy_code  <- data.table::fread( # {{{
        "
            World	A
            Developed economies	AA
            Europe	AAA
            European Union	AAAA
            Austria	AAAA01
            Belgium	AAAA02
            Belgium / Luxembourg	AAAA03
            Bulgaria	AAAA04
            Croatia	AAAA05
            Cyprus	AAAA06
            Czechia	AAAA07
            Denmark	AAAA08
            Estonia	AAAA09
            Finland	AAAA10
            France	AAAA11
            Germany	AAAA12
            Greece	AAAA13
            Hungary	AAAA14
            Ireland	AAAA15
            Italy	AAAA16
            Latvia	AAAA17
            Lithuania	AAAA18
            Luxembourg	AAAA19
            Malta	AAAA20
            Netherlands	AAAA21
            Poland	AAAA22
            Portugal	AAAA23
            Romania	AAAA24
            Slovakia	AAAA25
            Slovenia	AAAA26
            Spain	AAAA27
            Sweden	AAAA28
            Other Europe	AAAB
            Albania	AAAB01
            Belarus	AAAB02
            Bosnia and Herzegovina	AAAB03
            Iceland	AAAB04
            Moldova, Republic of	AAAB05
            Montenegro	AAAB06
            North Macedonia	AAAB07
            Norway	AAAB08
            Russian Federation	AAAB09
            Serbia	AAAB10
            Switzerland	AAAB11
            Ukraine	AAAB12
            United Kingdom	AAAB13
            North America	AAB
            Canada	AAB01
            United States	AAB02
            Other developed economies	AAC
            Australia	AAC01
            Bermuda	AAC02
            Israel	AAC03
            Japan	AAC04
            Korea, Republic of	AAC05
            New Zealand	AAC06
            Developing economies	AB
            Africa	ABA
            North Africa	ABAA
            Algeria	ABAA01
            Egypt	ABAA02
            Libya	ABAA03
            Morocco	ABAA04
            Sudan	ABAA05
            Tunisia	ABAA06
            Other Africa	ABAB
            Central Africa	ABABA
            Burundi	ABABA01
            Cameroon	ABABA02
            Central African Republic	ABABA03
            Chad	ABABA04
            Congo	ABABA05
            Congo, Democratic Republic of	ABABA06
            Equatorial Guinea	ABABA07
            Gabon	ABABA08
            Rwanda	ABABA09
            São Tomé and Principe	ABABA10
            East Africa	ABABB
            Comoros	ABABB01
            Djibouti	ABABB02
            Eritrea	ABABB03
            Ethiopia	ABABB04
            Kenya	ABABB05
            Madagascar	ABABB06
            Mauritius	ABABB07
            Seychelles	ABABB08
            Somalia	ABABB09
            Uganda	ABABB10
            United Republic of Tanzania	ABABB11
            Southern Africa	ABABC
            Angola	ABABC01
            Botswana	ABABC02
            Eswatini	ABABC03
            Lesotho	ABABC04
            Malawi	ABABC05
            Mozambique	ABABC06
            Namibia	ABABC07
            South Africa	ABABC08
            Zambia	ABABC09
            Zimbabwe	ABABC10
            West Africa	ABABD
            Benin	ABABD01
            Burkina Faso	ABABD02
            Cabo Verde	ABABD03
            Côte d'Ivoire	ABABD04
            Gambia	ABABD05
            Ghana	ABABD06
            Guinea	ABABD07
            Guinea-Bissau	ABABD08
            Liberia	ABABD09
            Mali	ABABD10
            Mauritania	ABABD11
            Niger	ABABD12
            Nigeria	ABABD13
            Senegal	ABABD14
            Sierra Leone	ABABD15
            Togo	ABABD16
            Asia	ABB
            East and South-East Asia	ABBA
            East Asia	ABBAA
            China	ABBAA01
            Hong Kong, China	ABBAA02
            Korea, Democratic People's Republic of	ABBAA03
            Macao, China	ABBAA04
            Mongolia	ABBAA05
            Taiwan Province of China	ABBAA06
            South-East Asia	ABBAB
            Brunei Darussalam	ABBAB01
            Cambodia	ABBAB02
            Indonesia	ABBAB03
            Lao People's Democratic Republic	ABBAB04
            Malaysia	ABBAB05
            Myanmar	ABBAB06
            Philippines	ABBAB07
            Singapore	ABBAB08
            Thailand	ABBAB09
            Timor-Leste	ABBAB10
            Viet Nam	ABBAB11
            South Asia	ABBAC
            Afghanistan	ABBAC01
            Bangladesh	ABBAC02
            Bhutan	ABBAC03
            India	ABBAC04
            Iran, Islamic Republic of	ABBAC05
            Maldives	ABBAC06
            Nepal	ABBAC07
            Pakistan	ABBAC08
            Sri Lanka	ABBAC09
            West Asia	ABBAD
            Armenia	ABBAD01
            Azerbaijan	ABBAD02
            Bahrain	ABBAD03
            Georgia	ABBAD04
            Iraq	ABBAD05
            Jordan	ABBAD06
            Kuwait	ABBAD07
            Lebanon	ABBAD08
            Oman	ABBAD09
            Qatar	ABBAD10
            Saudi Arabia	ABBAD11
            State of Palestine	ABBAD12
            Syrian Arab Republic	ABBAD13
            Turkey	ABBAD14
            United Arab Emirates	ABBAD15
            Yemen	ABBAD16
            Central Asia	ABBAE
            Kazakhstan	ABBAE01
            Kyrgyzstan	ABBAE02
            Tajikistan	ABBAE03
            Turkmenistan	ABBAE04
            Uzbekistan	ABBAE05
            Latin America and the Caribbean	ABC
            South and Central America	ABCA
            South America	ABCAA
            Argentina	ABCAA01
            Bolivia, Plurinational State of	ABCAA02
            Brazil	ABCAA03
            Chile	ABCAA04
            Colombia	ABCAA05
            Ecuador	ABCAA06
            Guyana	ABCAA07
            Paraguay	ABCAA08
            Peru	ABCAA09
            Suriname	ABCAA10
            Uruguay	ABCAA11
            Venezuela, Bolivarian Republic of	ABCAA12
            Central America	ABCAB
            Belize	ABCAB01
            Costa Rica	ABCAB02
            El Salvador	ABCAB03
            Guatemala	ABCAB04
            Honduras	ABCAB05
            Mexico	ABCAB06
            Nicaragua	ABCAB07
            Panama	ABCAB08
            Caribbean	ABCB
            Anguilla	ABCB01
            Antigua and Barbuda	ABCB02
            Aruba	ABCB03
            Bahamas	ABCB04
            Barbados	ABCB05
            British Virgin Islands	ABCB06
            Cayman Islands	ABCB07
            Curaçao	ABCB08
            Dominica	ABCB09
            Dominican Republic	ABCB10
            Grenada	ABCB11
            Haiti	ABCB12
            Jamaica	ABCB13
            Montserrat	ABCB14
            Netherlands Antilles	ABCB15
            Saint Kitts and Nevis	ABCB16
            Saint Lucia	ABCB17
            Saint Vincent and the Grenadines	ABCB18
            Sint Maarten	ABCB19
            Trinidad and Tobago	ABCB20
            Oceania	ABD
            Cook Islands	ABD01
            Fiji	ABD02
            French Polynesia	ABD03
            Kiribati	ABD04
            Marshall Islands	ABD05
            Micronesia, Federated States of	ABD06
            New Caledonia	ABD07
            Palau	ABD08
            Papua New Guinea	ABD09
            Samoa	ABD10
            Solomon Islands	ABD11
            Tonga	ABD12
            Tuvalu	ABD13
            Vanuatu	ABD14
            Least developed countries (LDCs)	AD
            Landlocked countries (LLCs)	AE
            Small island developing states (SIDS)	AF
        " , head = FALSE)  # }}}
data.table::setnames(economy_code, ori$mainkey[c("name", "code")])[, no := .I]

# 辅助函数 ============================================================== {{{1
ParseName <- function(name) {
    name <- stringr::str_split(name, "_")[[1]]
    info <- ori
    for (i in seq_along(name)) {
        info <- info[[name[i]]]
    }
    return(info)
}

NameRepair <- function(nms) {
    nms <- tolower(gsub("[./]", "_", nms))
    nms <- gsub("^([0-9]+)$", "yr_\\1", nms)
    return(nms)
}

#' @export
GetEconomyCode <- function(economy) {
    return(economy_code)
}

#' @export
GetDataInfo <- function() {
    return(ori)
}

#' @export
GetData <- function(name) {
    info <- ParseName(name)
    orifile <- file.path(ori$cache, paste0(name, ".xlsx"))

    if (!file.exists(orifile)) {
        utils::download.file(paste0(ori$lib, info$file), orifile)
        info$update <- Sys.Date()
    } else {
        info$update <- as.Date(file.mtime(orifile))
    }
    info$unit <- readxl::read_excel(orifile,
                                    range = gettextf("A%d:A%d", ori$unitrow, ori$unitrow),
                                    col_names = "unit") %>%
                 .[["unit"]]

    dt <- readxl::read_excel(orifile, skip = ori$unitrow, .name_repair = NameRepair)
    dt <- data.table::setDT(dt) %>%
        merge(economy_code, by = ori$mainkey['name'], all.x = FALSE, all.y = TRUE) %>%
        data.table::melt(ori$mainkey, variable.name = ori$time, value.name = name)
    dt[[ori$time]] <- sub("^yr_", "", dt[[ori$time]]) %>% as.integer()
    dt[, c(ori$time) := as.integer(sub("^yr_", "", .SD[[..ori$time]]))]
    dt %>% data.table::setattr("Description",      info$desc)   %>%
           data.table::setattr("Last Update Date", info$update) %>%
           data.table::setattr("Unit",             info$unit)

    return(dt)
}
