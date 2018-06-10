#' @return
#' A data.frame (tibble)
#'
#' @importFrom tibble as_tibble
#' @export
host_info <- function(system_info = NULL, qhost = NULL) {
  ## To please R CMD check
  field  <- hostname <- mounted_on <- NULL

  if (is.null(system_info)) system_info <- system_info()
  stopifnot(is.list(system_info))
  if (is.null(qhost)) qhost <- qhost()
  stopifnot(is.data.frame(qhost))

  qhost <- subset(qhost, hostname == system_info$hostname)
  stopifnot(nrow(qhost) == 1L)

  ## Operating system
  os <- subset(system_info$os_info, field %in% c("sysname", "release"))$value
  names(os) <- c("os", "os_release")
  os <- as.list(os)
  
  ## CPU
  ci <- system_info$cpu_info
  cpu_mhz <- subset(ci, field == "cpu_mhz")$value
  cpu_mhz <- gsub("[^0-9]*([0-9.]+)", "\\1", cpu_mhz)
  cpu_max <- max(as.double(cpu_mhz), na.rm = TRUE) / 1000
  cpu <- list(cpu_max = cpu_max)

  ## Disk
  df <- system_info$disk_free
  disk <- list(
    scratch = subset(df, mounted_on == "/scratch")$blocks_1024 * 1024 / 1024^3,
    tmp     = subset(df, mounted_on == "/tmp")$blocks_1024 * 1024 / 1024^3
  )

  as_tibble(cbind(qhost, os, cpu, disk))
}
