#' Compile Host Information
#'
#' @param system_info (optional) A named list as returned by [system_info()].
#'
#' @param qhost (optional) A data.frame as returned by [qhost()].
#' If `TRUE` (default), then [qhost()] is queried.
#' If `FALSE`, then queue information is not reported.
#'
#' @return
#' A data.frame (tibble) with information combined based
#' on `system_info` and `qhost`.
#'
#' @importFrom tibble as_tibble
#' @export
host_info <- function(system_info = NULL, qhost = TRUE) {
  ## To please R CMD check
  field  <- hostname <- mounted_on <- NULL

  if (is.null(system_info)) system_info <- system_info()
  stopifnot(is.list(system_info))

  ## Queuing info
  if (is.logical(qhost)) {
    stopifnot(length(qhost) == 1L, !is.na(qhost))
    qhost <- if (qhost) qhost() else NULL
  } else {
    stopifnot(is.data.frame(qhost))
    qhost <- subset(qhost, hostname == system_info$hostname)
    stopifnot(nrow(qhost) == 1L)
  }

  ## Operating system
  os <- subset(system_info$os_info, field %in% c("sysname", "release"))$value
  names(os) <- c("os", "os_release")
  os <- as.list(os)
  os <- as.data.frame(os, stringsAsFactors = FALSE)
  
  ## CPU label and speed
  ci <- system_info$cpu_info
  ## Parse the model name, e.g. Intel(R) Core(TM) i7-8650U CPU @ 1.90GHz
  value <- subset(ci, field == "model_name", select = "value", drop = TRUE)
  pattern <- "^(.*)[ ]*@[ ]*([0-9.]+)[ ]*(|GHz)[ ]*$"
  cpu_label <- trim(gsub(pattern, "\\1", value))
  cpu_ghz <- trim(gsub(pattern, "\\2", value))
  cpu_ghz <- as.numeric(cpu_ghz)
  cpu <- data.frame(cpu_label = cpu_label, cpu_ghz = cpu_ghz, stringsAsFactors = FALSE)

  ## Disk
  df <- system_info$disk_free
  disk <- list()
  for (mount in c("/scratch", "/tmp")) {
    value <- subset(df, mounted_on == mount, select = "blocks_1024", drop = TRUE)
    if (length(value) == 0L) {
      value <- NA_real_
    } else {
      value <- (1024 / 1024^3) * value
    }

    name <- sub("^/", "", mount)
    disk[[name]] <- value
  }
  disk <- as.data.frame(disk, stringsAsFactors = FALSE)

  args <- list(qhost, os, cpu, disk)
  args <- args[!vapply(args, FUN = is.null, FUN.VALUE = TRUE)]
  info <- do.call(cbind, args = args)

  as_tibble(info)
}
