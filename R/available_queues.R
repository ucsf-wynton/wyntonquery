#' @importFrom readr read_table cols col_character col_double
#' @export
available_queues <- function() {
  col_types <- cols(
    queuename        = col_character(),
    qtype            = col_character(),
    `resv/used/tot.` = col_character(),
    load_avg         = col_double(),
    arch             = col_character(),
    states           = col_character()
  )
  
  df <- read_table(pipe("qstat -f | grep -vE '^---'"), col_types = col_types,
                   na = c("-NA-"))
  colnames(df) <- tolower(colnames(df))

  parts <- strsplit(df$queuename, split = "@", fixed = TRUE)
  df$queue <- sapply(parts, FUN = `[`, 1L)
  df$hostname <- sapply(parts, FUN = `[`, 2L)

  df
}
