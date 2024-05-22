#' @export
cross <- function(...)
{   
    #' 根据两向量生成对称面板数据
    l <- list(...)
    if (length(l) != 2L) stop("Need two vector", call. = FALSE)
    
    out <- data.frame(V1 = rep(l[[1]], each  = length(l[[2]])),
                      V2 = rep(l[[2]], times = length(l[[1]])))
    if (!is.null(names(l))) {
        names(out) <- ifelse(names(l) == "", names(out), names(l))
    }
    out
}

