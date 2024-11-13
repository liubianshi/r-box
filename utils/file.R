box::use(purrr)
box::use(fs)

#' 在使用 SSH 登录服务器后，在服务器向客户端推送文件
#' @export
push <- function(
  file,
  port = 2222,
  user = "liubianshi",
  dest = "/tmp",
  force = FALSE,
  open = TRUE,
  silent = FALSE,
  wait = FALSE
) {
  if (file.exists(file)) {
    system2(
      "ssh_push",
      c("--port", port,
        "--user", user,
        "--dest", dest,
        if (force)  "--force"  else NULL,
        if (open)   "--open"   else NULL,
        if (silent) "--silent" else NULL,
        file),
      wait = wait
    )
  } else {
    stop("file isn't exists")
  }
}

#' 判断文件路径是否指向同一文件
#' @export
is_same <- function(...) {
  files <- c(...)
  stopifnot(is.character(files) && length(files) >= 2)

  targets <- purrr::map_chr(files, Sys.readlink)
  # 有文件不存在
  if (any(is.na(targets))) return(FALSE)
  # 存在符号链接时，基于链接目标判断，目标相同，则认为是同一文件
  if (any(targets != "")) {
    return(do.call(is_same, as.list(ifelse(targets != "", targets, files))))
  }

  # 路径相同，则认为是同一文件
  files <- unique(files)
  if (length(files) == 1) return(TRUE)

  dev_inode <- purrr::map_chr(
    files,
    ~ do.call(paste, fs::file_info(.x)[1, c("device_id", "inode")])
  )
  return(all(dev_inode[-1] == dev_inode[1]))
}

