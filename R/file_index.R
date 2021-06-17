#' Index a file for faster access to parts of the file
#'
#' @param pathname (character) The file to be indexed.
#'
#' @param skip (numeric) The number of bytes to skip before start indexing.
#'
#' @param n_max (numeric) The maximum number of bytes to scan.
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
#' @example incl/make_file_index.R
#'
#' @importFrom utils file_test
#' @export
make_file_index <- function(pathname, skip = 0L, n_max = Inf, newline = "\n", bfr_size = 10e6) {
  stopifnot(length(pathname) == 1L, file_test("-f", pathname))
  stopifnot(length(skip) == 1L, is.numeric(skip), is.finite(skip),
            skip >= 0)
  stopifnot(length(n_max) == 1L, is.numeric(n_max), !is.na(n_max), n_max >= 0)
  stopifnot(length(newline) == 1L, is.character(newline), !is.na(newline))
  stopifnot(length(bfr_size) == 1L, is.numeric(bfr_size), is.finite(bfr_size),
            bfr_size > 0)

  nl <- charToRaw(newline)

  con <- file(pathname, open = "rb")
  on.exit(close(con))

  skip_org <- skip
  if (skip > 0) {
    skip <- skip - 1L
    if (skip > 0) seek(con, where = skip, origin = "start", rw = "read")
  }

  offset <- skip
  pos <- list(offset)
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
    if (offset > n_max) break
  }
  pos <- unlist(pos, use.names = FALSE)
  if (is.finite(n_max)) pos <- pos[pos <= n_max]
  pos
}

#' @param index (numeric vector) A sorted index of file byte positions.
#'
#' @param file A pathname to a \file{*.index} file to be
#' created or read from.
#'
#' @rdname make_file_index
#' @export
save_file_index <- function(index, file) {
  stopifnot(is.numeric(index))
  index <- as.double(index)
  writeBin(index, con = file, endian = "little")
}


#' @rdname make_file_index
#' @importFrom utils file_test
#' @export
read_file_index <- function(file) {
  stopifnot(file_test("-f", file))
  file_size <- file.size(file)
  n <- file_size/8
  readBin(con = file, what = double(0L), endian = "little", n = n)
}


#' Open a file connection at given byte position
#'
#' @param file A pathname to a \file{*.index} file to be
#' created or read from.
#'
#' @param position The file byte position where to position the connection
#' after being opened.
#'
#' @param open (character) Mode to open the file connection in, cf.
#' [base::file()].
#'
#' @param auto_close (logical) If TRUE, then the connection is automatically
#' closed when garbage collected.
#'
#' @return A file [base::connection] moved to position `position`.
#'
#' @importFrom utils file_test
#' @export
open_file_at <- function(file, position = 0, open = "rb", auto_close = TRUE) {
  stopifnot(file_test("-f", file))
  file_size <- file.size(file)
  stopifnot(is.numeric(position), position >= 0, position <= file_size)
  con <- file(file, open = open)
  if (auto_close) {
    env <- new.env()
    env$con <- con
    attr(con, "gcMe") <- env
    reg.finalizer(env, function(e) {
      try(close(e$con), silent = TRUE)
    })
  }
  if (position > 0L) seek(con, where = position, origin = "start")
  con
}

