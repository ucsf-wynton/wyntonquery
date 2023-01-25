#' @importFrom DBI dbExecute
#' @importFrom glue glue
dbDeleteDuplicates <- function(conn, name, by) {
  stopifnot(
    inherits(conn, "DBIConnection"),
    length(name) == 1L, is.character(name), !is.na(name), nzchar(name),
    length(by) >= 1L, is.character(by), !is.na(by), nzchar(by)
  )

  where <- sprintf("c.%s = %s.%s", by, name, by)
  where <- paste(where, collapse = " AND ")
  sql <- glue(paste(c(
    "DELETE FROM { name }",
    "WHERE EXISTS (",
    "  SELECT 1 FROM { name } c",
    "  WHERE { where } AND c.rowid < { name }.rowid",
    ");"
  ), collapse = "\n"))

  dbExecute(conn, sql)
}

#' @importFrom DBI dbWriteTable
#' @importFrom parallel splitIndices
#' @export
dbi_import_sge_accounting <- function(conn, pathname, chunk_size = 10000L, deduplicate = FALSE) {
  stopifnot(
    inherits(conn, "DBIConnection"),
    utils::file_test("-f", pathname),
    length(chunk_size) == 1L, is.numeric(chunk_size),
    is.finite(chunk_size), chunk_size > 0
  )

  message("dbi_import_sge_accounting() ...")
  on.exit(message("dbi_import_sge_accounting() ... done"))
  message(sprintf("- SGE accounting file: %s", pathname))

  index <- make_file_index(pathname, skip = 4L)
  njobs <- length(index)
  message(sprintf("- Number of jobs: %d", njobs))
  message(sprintf("- Number of jobs per chunk: %d", chunk_size))

  chunks <- splitIndices(njobs, ncl = max(1L, njobs / chunk_size))
  message(sprintf("- Number of chunks: %d", length(chunks)))

  for (kk in seq_along(chunks)) {
    chunk <- chunks[[kk]]
    offset <- index[chunk[1]]
    message(sprintf("Chunk #%d: index=%d, offset=%d", kk, chunk[1], offset))
    jobs <- read_sge_accounting(pathname, offset = offset, n_max = chunk_size)
    message(sprintf("Jobs read: %d", nrow(jobs)))
    jobs <- as.data.frame(jobs)
    message("Converted to data.frame")
    dbWriteTable(conn, "jobs", jobs, append = TRUE)
    message("Jobs written to database")
    jobs <- NULL
  }

  if (deduplicate) {
    message("Removing duplicates")
    dbDeleteDuplicates(conn, "jobs", by = c("job_number", "task_number"))
  }
  
  invisible(njobs)
} ## dbi_import_sge_accounting()
