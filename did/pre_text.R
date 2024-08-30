box::use(fixest[feols])
box::use(lbs[genformula])
box::use(magrittr[`%>%`, `%<>%`, `%T>%`])
box::use(stringr[glue = str_glue, str_replace, str_match, str_detect])
box::use(data.table[setDT, as.data.table, data.table, setnames, setcolorder])
box::use(purrr[map_dfc, walk])

# 暴露期命名
exposure_prefix = function(p) {
  default = c(pre = "Pre.", cur = "Treat.", post = "Post.")
  if (is.null(p)) return(default)
  stopifnot(is.character(p) && length(p) == 3L)
  names(p) <- c("pre", "cur", "post")
  return(p)
}

#' 为暴露时间命令，如将 `-1` 命名为 `Pre-1`，表示干预前一期
naming_exposure_list <- function(exposure, prefix) {
  prefix <- exposure_prefix(prefix)
  exposure  <- as.integer(exposure)
  treat_period_name_prefix <-
    ifelse(exposure <  0, prefix["pre"],
    ifelse(exposure == 0, prefix["cur"], prefix["post"]))
  paste0(treat_period_name_prefix, abs(exposure))
}

#' 将干预期分组
group_exposure_list <- function(exposure_list, combine) {
  if (is.null(combine)) {
    exposure_list <- as.list(exposure_list)
    names(exposure_list) <- as.character(exposure_list)
    return(exposure_list)
  }

  group_name <- function(times) {
    times_m <- min(times)
    times_M <- max(times)
    name <-
      if (times_m < 0 && times_M < 0) {
        times_M
      } else if (times_m > 0 && times_M > 0) {
        times_m
      } else {
        0
      }
    as.character(name)
  }

  groups <- list()
  for (i in seq_along(combine)) {
    times <- combine[[i]]
    times <-
      if (
        is.character(times) &&
        length(times) == 1L &&
        str_detect(times[1], "^\\s*[<>]=?\\s*[-]?[0-9]+\\s*$")
      ) {
            gettextf("exposure_list[exposure_list %s]", times[1]) %>%
            parse(text = .) %>%
            eval()
      } else {
        intersect(exposure_list, times)
      }
    groups[[group_name(times)]] <- times
    exposure_list <- setdiff(exposure_list, times)
  }

  for (.x in exposure_list) {
    groups[[as.character(.x)]] <- .x
  }

  return(groups) 
}

#' 基于观测时间和个体被干预的时间，生成一系列虚拟变量，$D_{it}^l$, 
#' 当时间 $t$ 距离首次被干预的时间 $t^0$ 正好等于 $l$ 时取 1
#' 返回行数等于 `length(time)` 的数据框
generate_exposure_dummy_list <- function(
  time, # Vector of response times to observations 
  treat_time, # Variables of time when an individual was first intervened 
  drop_exposure_list = -1, # base period 
  prefix = NULL,
  combine = NULL # group treat_time, integer list 
) {
  T_max <- max(time, na.rm = TRUE)
  T_min <- min(time, na.rm = TRUE)
  T_len <- T_max - T_min + 1

  exposure_list <-
    setdiff((-T_len):(T_len-1), drop_exposure_list) %>%
    group_exposure_list(combine)

  names(exposure_list) %<>%
    as.integer() %>%
    naming_exposure_list(prefix) 

  purrr::map_dfc(
    exposure_list,
    ~ {
      exposure_dummy <- !(is.na(treat_time) | is.na(match(time - treat_time, .x)))
      if (all(!exposure_dummy)) NULL else exposure_dummy 
    }
  )
}

#' 估计方程，返回系数的名称，系数估计值，标准差和自由度
estimate_exposure_coef <- function(data, dep, indep, fe, se = "cluster") {
    fml <- lbs::genformula(c(dep, indep), "fixest", fe)
    es <- fixest::feols(fml, data, se = se)
    data.table(
      var = str_replace(names(es$coefficients), "TRUE$", ""),
      coef = es$coefficients,
      se = es$se
    )
}

# 在实际的干预效应出现缺失时补全，方便作图
complete_exposure <- function(exposure, prefix = NULL) {
    prefix %<>% exposure_prefix()
    min <-
      str_match(exposure, paste0(prefix["pre"], "(\\d+)"))[,2]  %>%
      as.integer() %>%
      max(na.rm = TRUE)
    max <-
      str_match(exposure, paste0(prefix["post"], "(\\d+)"))[,2] %>%
      as.integer() %>%
      max(na.rm = TRUE)
    complete_exposure_list <- (-min):max
    names(complete_exposure_list) <- naming_exposure_list(complete_exposure_list, prefix)
    complete_exposure_list
}

#' Estimate treatment effect on the outcome variable using parallel trend
#' assumption
#'
#' This function estimates the treatment effect on the outcome variable using
#' the parallel trend assumption. It generates exposure dummy variables,
#' estimates exposure coefficients, and returns the complete exposure dummy
#' coefficients.
#'
#' @param data A data frame containing the variables specified in id, time, treat, output, covs.
#' @param id A character string specifying the ID variable.
#' @param time A character string specifying the time variable.
#' @param treat A character string specifying the treatment variable.
#' @param output A character string specifying the outcome variable.
#' @param never_treat A vector of values in the treat variable that should never receive treatment.
#' @param drop_exposure_list An integer specifying the exposure dummy variables to drop.
#' @param combine A list of exposure dummy variables to combine.
#' @param covs A character vector specifying the covariates.
#' @param prefix A character string specifying the prefix for the exposure dummy variables.
#'
#' @return A data table containing the complete exposure dummy coefficients.
#'
#' @export
parallel_trend <- function(
  data, id, time, treat, output,
  never_treat = NULL,
  drop_exposure_list = -1,
  combine = NULL,
  covs = NULL,
  prefix = NULL
) {
  sample <-
    as.data.table(data) %>%
    .[, .SD, .SDcols = c(id, time, treat, output, covs)] %>%
    na.omit(c(id, time, output, covs))

  old_var_name <- c(ID = id, Time = time, Treat = treat)
  setnames(sample, old_var_name, names(old_var_name))
  if (!is.null(never_treat)) {
      sample[!is.na(match(Treat, never_treat)), Treat := NA]
  }

  exposure_dummy <- generate_exposure_dummy_list(
    sample$Time, sample$Treat,
    prefix = prefix,
    drop_exposure_list = drop_exposure_list,
    combine = combine
  )
  covs_coef <- estimate_exposure_coef(
    data  = cbind(sample, exposure_dummy),
    dep   = output,
    indep = c(covs, names(exposure_dummy)),
    fe    = c("ID", "Time")
  )

  complete_exposure_list <-
    complete_exposure(covs_coef$var) %>%
    as.data.table(keep.rownames = TRUE) %>%
    setnames(c("var", "exposure"))

  complete_exposure_dummy_coefs <-
    covs_coef[complete_exposure_list, on = "var", nomatch = NA] %>%
    .[is.na(coef), `:=`(coef = 0, se = 0)] %>%
    setcolorder(c("var", "exposure", "coef", "se"))

  complete_exposure_dummy_coefs
}

