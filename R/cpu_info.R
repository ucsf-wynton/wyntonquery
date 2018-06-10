#' Lists the CPU Information for the Current Machine
#'
#' @return
#' A data.frame (tibble) with the two columns `field` and `value`.
#' For most CPUs, there will be duplicated `field`:s.
#'
#' @details
#' This function reads/queries `/proc/cpuinfo` on the current machine.
#'
#' @examples
#' info <- cpu_info()
#' print(info)
#'
#' @seealso [lscpu()]
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
  field <- gsub(pattern, "\\1", bfr)
  field <- gsub(":", "", field, fixed = TRUE)
  field <- gsub(" ", "_", tolower(field), fixed = TRUE)
  tibble(field = field, value = gsub(pattern, "\\2", bfr))
}
