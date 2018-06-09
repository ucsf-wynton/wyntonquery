#' Lists System Information for the Current Machine
#'
#' @return
#' A named list with elements `hostname` (a character string),
#' `os_info`, `cpu_info`, and `disk_free` (data frames/tibbles).
#'
#' @details
#' This function reads/queries [base::Sys.info()], [disk_free()],
#' and [cpu_info()] on the current machine.
#'
#' @examples
#' info <- system_info()
#' print(info)
#'
#' @importFrom tibble tibble
#' @export
system_info <- function() {
  si <- as.list(Sys.info())
  hostname <- si[["nodename"]]
  si <- si[!grepl("(user|login|nodename)", names(si))]
  si$os_type <- .Platform[["OS.type"]]
  si <- tibble(field = names(si), value = unlist(si, use.names = FALSE))
  list(
    hostname    = hostname,
    os_info     = si,
    cpu_info    = cpu_info(),
    disk_free   = disk_free(local = TRUE)
  )
}

