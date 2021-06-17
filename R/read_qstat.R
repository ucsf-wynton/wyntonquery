#' Get information on jobs currently on the SGE queue
#'
#' @param file A file, a connection, or NULL.  If NULL (default), then
#' the output of system call `qstat -xml -u '*'` is read.
#'
#' @return A [tibble::tibble]
#'
#' @importFrom xml2 as_list read_xml
#' @importFrom tibble as_tibble
#' @importFrom dplyr left_join
#' @importFrom readr write_tsv read_tsv cols
#' @importFrom readr col_character col_double col_integer col_datetime
#' @export
read_qstat <- function(file = NULL) {
  if (is.null(file)) {
    file <- pipe("qstat -xml -u '*'")
    on.exit(close(file))
  }
  x <- read_xml(file)
  x <- as_list(x)
  x <- x$job_info
  stopifnot(length(x) == 2)

  y <- lapply(x, FUN = function(z) {
    z <- unname(z)
    z <- lapply(z, FUN = unlist)
    names <- lapply(z, FUN = names)
    names <- unlist(names, use.names = FALSE)
    names <- unique(names)
    z <- lapply(z, FUN = `[`, names)
    z <- do.call(rbind, z)
    colnames(z) <- names
    as_tibble(z)
  })
  
  ## Coerce to proper data types
  queue_info <- local({
    con <- rawConnection(raw(), open = "wb")
    on.exit(close(con))
    write_tsv(y$queue_info, file = con)
    raw <- rawConnectionValue(con)
    col_types <- cols(
      JB_job_number = col_double(),
      JAT_prio = col_double(),
      JB_name = col_character(),
      JB_owner = col_character(),
      state = col_character(),
      JAT_start_time = col_datetime(format = ""),
      queue_name = col_character(),
      slots = col_integer(),
      tasks = col_character()
    )
    read_tsv(raw, col_types = col_types)
  })

  job_info <- local({
    con <- rawConnection(raw(), open = "wb")
    on.exit(close(con))
    write_tsv(y$job_info, file = con)
    raw <- rawConnectionValue(con)
    col_types <- cols(
      JB_job_number = col_double(),
      JAT_prio = col_double(),
      JB_name = col_character(),
      JB_owner = col_character(),
      state = col_character(),
      JB_submission_time = col_datetime(format = ""),
      slots = col_integer(),
      tasks = col_character()
    )
    read_tsv(raw, col_types = col_types)
  })

  ## We use a left join here, because some of the jobs in 'queue_info'
  ## are defunct, e.g. jobs that where submitted a very long time ago
  ## and no longer exists
  by <- c("JB_job_number", "JB_name", "JB_owner", "slots")
  qstat <- left_join(job_info, queue_info, by = by)
  colnames(qstat) <- gsub("^(JB|JAT)_", "", colnames(qstat))
  class(qstat) <- c("qstat_xml", class(qstat))
  
  qstat
}  


#' @importFrom readr read_tsv
read_qstat_ext <- function(file = NULL) {
  if (is.null(file)) {
    file <- pipe("qstat -ext -g d -u '*'")
    on.exit(close(file))
  }
  bfr <- readLines(file)
  bfr <- grep("^-+$", bfr, invert = TRUE, value = TRUE)
  bfr <- gsub("^[ ]+", "", bfr)
  bfr <- gsub(" +", "\t", bfr)
  bfr[1] <- gsub("\t$", "", bfr[1])
  jobs <- read_tsv(bfr, col_names = TRUE)
  colnames(jobs) <- gsub("-", "_", colnames(jobs))
  class(jobs) <- c("qstat_raw", class(jobs))
  jobs
}
