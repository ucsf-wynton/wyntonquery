#' Report on File System Disk Usage
#'
#' @param mounts A character vector of disks (mount points) to query.
#' If `NULL`, all disks are queried.
#'
#' @param local If `TRUE`, only local disks are queried.
#'
#' @return
#' A data.frame (tibble) where each row corresponds to a unique disk.
#'
#' @details
#' This function queries `df -P` (POSIX output format) on the system.
#'
#' @examples
#' ## All disks
#' df <- disk_free()
#' print(df)
#'
#' ## All local disks
#' df <- disk_free(local = TRUE)
#' print(df)
#'
#' ## /tmp 
#' df <- disk_free(c("/tmp"))
#' print(df)
#'
#' @importFrom readr read_table cols col_character col_double
#' @export
disk_free <- function(mounts = NULL, local = FALSE) {
  bin <- Sys.which("df")
  stopifnot(file_test("-f", bin), file_test("-x", bin))

  args <- c("-P")
  if (local) args <- c(args, "-l")
  if (!is.null(mounts)) args <- c(args, mounts)

  bfr <- system2(bin, args = args, stdout = TRUE)
  status <- attr(bfr, "status")
  if (!is.null(status)) {
    stop(sprintf("Failed to query %s. Status code: %s",
                 paste(c(bin, args), collapse = " "), status))
  }

  ## Prune data so it's parsable
  bfr <- gsub("Mounted on", "Mounted_on", bfr, fixed = TRUE)
  bfr <- gsub("[[:space:]]+", ",", bfr)

  ## Parse
  col_types <- cols(
    Filesystem    = col_character(),
    Used          = col_double(),
    Available     = col_double(),
    `Mounted_on`  = col_character()
  )
  df <- readr::read_csv(I(bfr), col_names = TRUE, col_types = col_types)

  names <- tolower(colnames(df))
  names[names %in% c("1024-blocks", "1k-blocks")] <- "blocks_1024"
  names <- gsub("%", "", names)
  colnames(df) <- names

  ## Parse percentage
  for (name in names) {
    value <- df[[name]]
    if (!is.character(value)) next
    if (!all(grepl("%$", value))) next
    value <- gsub("%$", "", value)
    value <- as.numeric(value) / 100
    df[[name]] <- value
  }
  
  df
}

