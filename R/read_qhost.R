#' @importFrom readr read_table cols col_character col_integer col_double
#' @export
read_qhost <- function() {
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
  df
}
