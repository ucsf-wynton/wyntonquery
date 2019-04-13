#' List Available Host and Their Current Statuses
#'
#' @return
#' A data.frame (tibble) where each row corresponds to a unique host
#' (compute node).
#'
#' @examples
#' \donttest{
#' qh <- qhost()
#' print(qh)
#'
#' ## Ignore compute nodes that are without load
#' qh <- subset(qh, !is.na(load))
#' print(qh)
#' }
#'
#' @details
#' This function queries SGE's `qhost` on the system.
#'
#' @importFrom readr read_table cols col_character col_integer col_double
#' @export
qhost <- function() {
  col_types <- cols(
    HOSTNAME = col_character(),
    ARCH = col_character(),
    NCPU = col_integer(),
    NSOC = col_integer(),
    NCOR = col_integer(),
    NTHR = col_integer(),
    LOAD = col_double(),
    MEMTOT = col_character(),
    MEMUSE = col_character(),
    SWAPTO = col_character(),
    SWAPUS = col_character()
  )
  
  df <- read_table(pipe("qhost | awk '(NR != 2)'"), col_types = col_types)
  colnames(df) <- tolower(colnames(df))

  scale <- c(K = 1000, M = 1000^2, G = 1000^3, T = 1000^4)
  for (field in c("memtot", "memuse", "swapto", "swapus")) {
    values <- df[[field]]
    for (suffix in c("K", "M", "G", "T")) {
      idx <- grep(suffix, values)
      if (length(idx) == 0) next
      values[idx] <- as.numeric(gsub(suffix, "", values[idx])) * scale[suffix]
    }
    df[[field]] <- suppressWarnings(as.numeric(values) / 1000^3)
  }
  
  df
}
