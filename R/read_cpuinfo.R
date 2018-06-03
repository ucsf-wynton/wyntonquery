#' @importFrom tibble tibble
#' @export
read_cpuinfo <- function() {
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
