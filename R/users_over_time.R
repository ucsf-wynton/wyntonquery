#' The number of Wynton users over time
#'
#' @param file A file with a single column of signup dates, or NULL.
#' If NULL, then the Wynton LDAP server is queried.
#'
#' @param since Drop signup dates prior to this date.
#'
#' @return A [tibble::tibble] with columns `date` and `total`,
#' `total` the cumulative sum based on `date` occurances.
#'
#' @example incl/users_over_time.R
#'
#' @importFrom readr read_tsv cols col_date
#' @importFrom tibble as_tibble
#' @export
users_over_time <- function(file = NULL, since = "2017-01-01") {
  if (is.null(file)) {
    file <- pipe("ldapsearch -H 'ldap://m1,ldap://m2' -x wyntonAccess=TRUE -x -LLL + | grep -E '^(createTimestamp:)' | sed 's/.* //' | sed -E 's/([0-9]{4})([0-9]{2})([0-9]{2}).*/\1-\2-\3/'")
    on.exit(close(file))
  }

  data <- read_tsv(file, col_names = "date", cols(date = col_date(format = "")))
  data <- cbind(data, total = seq_len(nrow(data)))
  if (!is.null(since)) data <- subset(data, date >= since)
  data <- as_tibble(data)
  
  data
}
