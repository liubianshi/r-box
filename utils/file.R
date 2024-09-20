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

