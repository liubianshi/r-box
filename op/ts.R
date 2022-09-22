#' test
#' @export
changerate <- function(x, t, before, after, .fun = NULL)
{ # 计算变化率
    dt <- data.table::data.table(x = x, t = t)
    m1 <- mean(x[t %in% before], na.rm = TRUE)
    m2  <- mean(x[t %in% after],  na.rm = TRUE)
    if (is.null(.fun)) .fun = function(x1, x2) x2 / x1 - 1
    .fun(m1, m2)
}
