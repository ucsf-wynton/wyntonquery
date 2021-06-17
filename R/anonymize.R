#' Anonymize fields in a data.frame
#'
#' @param x The [base::data.frame] to be anonymized.
#'
#' @param fields (character vector) Columns to be anonymized.
#'
#' @param \dots Not used.
#'
#' @return A [base::data.frame] of the same dimensions as `code`.
#'
#' @export
anonymize <- function(x, fields, ...) {
  UseMethod("anonymize")
}


#' @importFrom forcats fct_anon
#' @export
anonymize.data.frame <- function(x, fields, ...) {
  stopifnot(is.character(fields), !anyNA(fields), all(nzchar(fields)))
  stopifnot(all(fields %in% colnames(x)))
  
  for (name in fields) {
    x[[name]] <- as.character(fct_anon(as.factor(x[[name]]), prefix = name))
  }
  
  x
}


#' @export
anonymize.raw_sge_accounting <- function(x, fields = c("group", "owner", "project"), ...) {
  NextMethod(fields = fields)
}


#' @export
anonymize.qstat_ext <- function(x, fields = c("user", "project", "department"), ...) {
  NextMethod(fields = fields)
}


#' @export
anonymize.qstat_xml <- function(x, fields = c("owner"), ...) {
  NextMethod(fields = fields)
}
