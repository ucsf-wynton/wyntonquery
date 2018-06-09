#' Lists the CPU Information for the Current Machine
#'
#' @return
#' A data.frame (tibble) with the two columns `field` and `value`.
#'
#' @details
#' This function reads/queries `/proc/cpuinfo` on the current machine.
#'
#' @examples
#' info <- cpu_info()
#' print(info)
#'
#' @importFrom tibble tibble
#' @export
cpu_info <- function() {
  bfr <- readLines("/proc/cpuinfo")
  bfr <- gsub("\t", "", bfr)
  bfr <- gsub("[ ]+", " ", bfr)
  bfr <- bfr[nzchar(bfr)]
  bfr <- unique(bfr)
  bfr <- c(sprintf("hostname: %s", Sys.info()[["nodename"]]), bfr)
  pattern <- "([a-z ]+):[ ]+(.*)"
  tibble(field = gsub(pattern, "\\1", bfr),
         value = gsub(pattern, "\\2", bfr))
}
