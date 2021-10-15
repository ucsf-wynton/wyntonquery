#' Open a file connection at given byte position
#'
#' @param file A pathname to a \file{*.index} file to be
#' created or read from.
#'
#' @param offset The file byte position where to position the connection
#' after being opened.
#'
#' @param open (character) Mode to open the file connection in, cf.
#' [base::file()].
#'
#' @param auto_close (logical) If TRUE, then the connection is automatically
#' closed when garbage collected.
#'
#' @return A file [base::connection] moved to position `offset`.
#'
#' @importFrom utils file_test
#' @export
open_file_at <- function(file, offset = 0, open = "rb", auto_close = TRUE) {
  stopifnot(file_test("-f", file))
  file_size <- file.size(file)
  stopifnot(is.numeric(offset), offset >= 0, offset <= file_size)
  con <- file(file, open = open)
  if (auto_close) {
    env <- new.env()
    env$con <- con
    attr(con, "gcMe") <- env
    reg.finalizer(env, function(e) {
      try(close(e$con), silent = TRUE)
    })
  }
  if (offset > 0L) seek(con, where = offset, origin = "start")
  con
}
