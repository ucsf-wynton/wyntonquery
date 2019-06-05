#' Produce a Table of Static Host Information
#'
#' @param host_info A data.frame as returned by one or more
#' [host_info()] calls.
#'
#' @param format If `TRUE`, the output is formatted to strings
#' with units and friendly column names.
#'
#' @return A data.frame.
#'
#' @export
host_table <- function(host_info, format = TRUE) {
  data <- host_info[, c("hostname", "ncor", "cpu", "memtot", "scratch", "tmp")]
  data$memtot <- round_ram(data$memtot)
  data$scratch <- signif(data$scratch / 1024, digits = 2L)
  data$tmp <- signif(data$tmp, digits = 2L)

  if (format) {
    data$memtot  <- sprintf("%.0f GiB", data$memtot)
    data$scratch <- sprintf("%.1f TiB", data$scratch)
    data$tmp     <- sprintf("%.1f GiB", data$tmp)
    colnames(data) <- c("Node", "# Physical Cores", "CPU", "RAM", "Local `/scratch`", "Local `/tmp`")
  }
  
  data
}
