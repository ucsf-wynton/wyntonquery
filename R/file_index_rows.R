#' Index the beginning of rows in a file
#'
#' @param pathname (character) The file to be indexed.
#'
#' @param newline (character) The character to scan for.
#'
#' @param bfr_size (numeric) The number of bytes to read in each iteration.
#'
#' @return A numeric vector of file byte offsets that corresponds to the
#' beginning of a line, i.e. a position in the file that was preceeded
#' by a `newline` character.  The first line is at file byte offset
#' `0`, which is also always the first element in the returned vector.
#'
#' @importFrom utils file_test
#' @export
file_index_rows <- function(pathname, newline = "\n", bfr_size = 10000L) {
  stopifnot(length(pathname) == 1L, file_test("-f", pathname))
  stopifnot(length(newline) == 1L, is.character(newline), !is.na(newline))
  stopifnot(length(bfr_size) == 1L, is.numeric(bfr_size), is.finite(bfr_size),
            bfr_size > 0)
            
  con <- file(pathname, open = "rb")
  on.exit(close(con))

  nl <- charToRaw(newline)
  offset <- 0
  pos <- list(0)
  repeat {
    raw <- readBin(con, what = raw(), n = bfr_size)
    nraw <- length(raw)
    ## Reached end of file?
    if (nraw == 0) break
    idxs <- which(raw == nl)
    raw <- NULL
    idxs <- idxs + offset
    offset <- offset + nraw
    pos[[length(pos) + 1]] <- idxs
    idxs <- NULL
  }
  pos <- unlist(pos, use.names = FALSE)
  pos
}
