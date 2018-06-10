#' Lists the Memory Information for the Current Machine
#'
#' @return
#' A data.frame (tibble) with the two columns `field` and `value`.
#' For most CPUs, there will be duplicated `field`:s.
#'
#' @details
#' This function reads/queries `/proc/meminfo` on the current machine.
#'
#' @examples
#' info <- mem_info()
#' print(info)
#'
#' @importFrom tibble tibble
#' @export
mem_info <- function() {
  bfr <- readLines("/proc/meminfo")
  pattern <- "([^ ]+):[ ]+(.*)"
  field <- gsub(pattern, "\\1", bfr)
  field <- gsub(" ", "_", tolower(field), fixed = TRUE)
  value <- gsub(pattern, "\\2", bfr)
  value <- gsub(" kB", "000", value)
  value <- as.numeric(value)
  tibble(field = field, value = value)
}
