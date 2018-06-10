#' Lists the CPU Information for the Current Machine
#'
#' @return
#' A data.frame (tibble) with the two columns `field` and `value`.
#' For most CPUs, there will be duplicated `field`:s.
#'
#' @details
#' This function queries `lscpu` on the current machine.
#'
#' @examples
#' info <- lscpu()
#' print(info)
#'
#' @seealso [cpu_info()]
#'
#' @importFrom readr read_table cols col_character
#' @export
lscpu <- function() {
  col_types <- cols(
    X1 = col_character(),
    X2 = col_character()
  )
  info <- read_table(pipe("lscpu"), col_names = FALSE, col_types = col_types)
  colnames(info) <- c("field", "value")
  info$field <- gsub(":", "", info$field, fixed = TRUE)
  info$field <- gsub(" ", "_", tolower(info$field), fixed = TRUE)
  info$field <- gsub("-", "_", tolower(info$field), fixed = TRUE)
  info$field <- gsub("(s)", "s", info$field, fixed = TRUE)
  info
}
