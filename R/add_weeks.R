#' Append week timestamps fields in a data.frame
#'
#' @param x The [base::data.frame] to be weekified.
#'
#' @param fields (character vector) Columns to be weekified.
#'
#' @param \dots Not used.
#'
#' @return A [base::data.frame] of the same dimensions as `code`.
#'
#' @export
add_weeks <- function(x, fields, ...) {
  UseMethod("add_weeks")
}


#' @importFrom forcats fct_anon
#' @export
add_weeks.data.frame <- function(x, fields, ...) {
  stopifnot(is.character(fields), !anyNA(fields), all(nzchar(fields)))
  stopifnot(all(fields %in% colnames(x)))
  
  for (name in fields) {
    week_name <- sprintf("%s_week", name)
    x[[week_name]] <- format(x[[name]], format = "%GW%V")
  }
  
  x
}

#' @export
add_weeks.raw_sge_accounting <- function(x, fields = c("submission_time", "start_time", "end_time"), ...) {
  NextMethod(fields = fields)
}

#' @export
add_weeks.sge_accounting <- function(x, fields = c("submission_time", "start_time", "end_time"), ...) {
  NextMethod(fields = fields)
}
