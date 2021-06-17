#' Get information on jobs currently on the SGE queue
#'
#' @return A [tibble::tibble]
#'
#' @importFrom readr read_tsv
#' @export
read_qstat <- function() {
  con <- pipe("qstat -ext -g d -u '*'")
  on.exit(close(con))
  bfr <- readLines(con)
  bfr <- grep("^-+$", bfr, invert = TRUE, value = TRUE)
  bfr <- gsub("^[ ]+", "", bfr)
  bfr <- gsub(" +", "\t", bfr)
  bfr[1] <- gsub("\t$", "", bfr[1])
  jobs <- read_tsv(bfr, col_names = TRUE)
  colnames(jobs) <- gsub("-", "_", colnames(jobs))
  class(jobs) <- c("qstat", class(jobs))
  jobs
}


#' @param fields (character vector) Columns to be anonymized.
#'
#' @rdname read_raw_sge_accounting
#' @importFrom forcats fct_anon
#' @export
anonymize_qstat <- function(x, fields = c("user", "project", "department")) {
  stopifnot(inherits(x, "qstat"))
  stopifnot(all(fields %in% colnames(x)))
  
  for (name in fields) {
    x[[name]] <- as.character(fct_anon(as.factor(x[[name]]), prefix = name))
  }
  
  x
}
